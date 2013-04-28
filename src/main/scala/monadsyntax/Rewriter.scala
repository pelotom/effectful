package monadsyntax

import language.experimental.macros
import language.higherKinds
import reflect.macros.Context
import scalaz.{Monad, Unapply}
import scalaz.std.list.groupWhen
import Function.unlift

/**
 * Transforms the AST of an argument to `monadically`, rewriting `unwrap` calls
 * using `>>=` and `pure`. 
 */
private abstract class Rewriter {

  val c: Context

  import c.universe._

  val TMPVAR_PREFIX = "$tmc$"

  val unapplyInstance: Tree = {
    val tree = c.macroApplication
    resolveUnapply(tree.tpe) getOrElse inferUnapplyOrFail(tree)
  }
  
  val unapplyName = newTermName(TMPVAR_PREFIX + "unapply")
  
  val monadInstance = getTypeClass(Ident(unapplyName))
  
  def inferUnapplyOrFail(tree: Tree): Tree = {
    val unapplies = collectUnwrapArgs(tree) map (_._2)
    
    if (unapplies.isEmpty)
      c.abort(tree.pos, s"could not infer the monadic type being used here because $UNWRAP is never used")
      
    val instanceTypes = groupWhen(unapplies.map(u => c.typeCheck(getTypeClass(u)).tpe))(_=:=_).map(_(0))
    if (instanceTypes.size > 1)
      c.abort(tree.pos, s"cannot unwrap more than one monadic type in a given $MONADICALLY block")

    unapplies(0)
  }
  
  /**
   * Strips out implicit conversions and implicit arguments, which complicate the AST and
   * make it difficult to detect for-comprehensions.
   */
  def stripImplicits(tree: Tree): Tree = new Transformer {
    override def transform(tree: Tree): Tree = {
      import scala.reflect.internal.Trees
      val isImplicitConversion = tree.isInstanceOf[Trees#ApplyImplicitView]
      val hasImplicitArgs = tree.isInstanceOf[Trees#ApplyToImplicitArgs]
      tree match {
        case Apply(fun, _)
          if hasImplicitArgs
          && !isUnwrap(fun)
          && !isMonadicToUnwrappable(fun) => transform(fun)
        case Apply(fun, List(arg))
          if isImplicitConversion
          && !isMonadicToUnwrappable(fun) => transform(arg)
        case _ => super.transform(tree)
      }
    }
  }.transform(tree)
  
  /**
   * Collects all arguments of `unwrap` or its postfix equivalents within the given tree.
   */
  def collectUnwrapArgs(tree: Tree): List[(Tree, Tree)] = {
    import scala.collection.mutable.ListBuffer
    val buf: ListBuffer[(Tree, Tree)] = ListBuffer()
    new Traverser() {
      override def traverse(tree: Tree) = getUnwrapArgs(tree) match {
        case Some(args) => buf += args
        case None => super.traverse(tree)
      }
    }.traverse(tree)
    buf.toList
  }

  def addUnapplyToScope(tree: Tree) = {
    val unapplyDecl = ValDef(Modifiers(), unapplyName, TypeTree(), unapplyInstance)
    Block(List(unapplyDecl), tree)
  }

  /**
   * The entry point of the algorithm, the meat of the work is done in `transform`.
   */
  def rewrite(tree: Tree): Tree = {
    var newTree = tree
    newTree = c.resetLocalAttrs(newTree)
    newTree = stripImplicits(newTree)
    newTree = transform(newTree)
    newTree = addUnapplyToScope(newTree)
    // println(show(newTree))
    newTree = c.typeCheck(newTree)//, WildcardType, true)
    val TypeRef(pre, sym, args) = c.macroApplication.tpe
    if (sym == typeOf[Nothing].typeSymbol)
      // fix up type inference
      c.macroApplication.setType(newTree.tpe)
    newTree
  }

  /**
   * Takes a tree representing an expression of type `A`, possibly containing `unwrap` calls,
   * and transforms it into a tree representing an expression of type `M[A]`, using monadic
   * operations.
   */
  def transform(tree: Tree): Tree = transform(extractBindings(tree))

  def transform(group: BindGroup, isPure: Boolean = true): Tree = group match { case(binds, tree) =>
    binds match {
      case Nil => 
        if (isPure) Apply(Select(monadInstance, newTermName("pure")), List(tree)) // make monadic
        else tree
      case (name, unwrappedFrom) :: moreBinds => 
        val innerTree = transform((moreBinds, tree), isPure)
        // q"$unwrappedFrom.flatMap($name => $innerTree)"
        val fun = Function(List(ValDef(Modifiers(Flag.PARAM), name, TypeTree(), EmptyTree)), innerTree)
        Apply(Apply(Select(monadInstance, newTermName("bind")), List(unwrappedFrom)), List(fun))
    }
  }

  def pkg = rootMirror.staticPackage("monadsyntax").asModule.moduleClass.asType.toType
  def wrapSymbol = pkg.member(newTermName(MONADICALLY))
  def unwrapSymbol = pkg.member(newTermName(UNWRAP))
  def conversionSymbol = pkg.member(newTermName(MONADIC_TO_UNWRAPPABLE))
  def isWrap(tree: Tree): Boolean = tree.symbol == wrapSymbol
  def isUnwrap(tree: Tree): Boolean = tree.symbol == unwrapSymbol
  def isMonadicToUnwrappable(tree: Tree): Boolean = tree.symbol == conversionSymbol

  type Binding = (TermName, Tree)

  /**
   * A `BindGroup` represents a sequence of monadic bindings and the Tree in which they
   * are to be bound.
   */
  type BindGroup = (List[Binding], Tree)

  /**
   * - Takes a tree for an expression of type A
   * - Makes a new tree in which all invocations of `unwrap` are replaced with
   *   fresh identifiers
   * - Returns all new bindings created, along with the new tree.
   * - The new tree still represents an expression of type A
   * - The terms of the bindings represent expressions of type M[A]
   */
  def extractBindings(tree: Tree): BindGroup = extractUnwrap(tree) orElse extractTraverse(tree) getOrElse { tree match {
    
    case Apply(fun, args) => 
      val (funBinds, newFun) = extractBindings(fun)
      val (argBindss, newArgs) = (args map extractBindings).unzip
      (funBinds ++ argBindss.flatten, Apply(newFun, newArgs))
    
    case TypeApply(fun, tyArgs) =>
      val (funBinds, newFun) = extractBindings(fun)
      (funBinds, TypeApply(newFun, tyArgs))
    
    case Select(tree, name) =>
      val (binds, newTree) = extractBindings(tree)
      (binds, Select(newTree, name))
    
    case ValDef(mod, lhs, typ, rhs) =>
      val (binds, newRhs) = extractBindings(rhs)
      (binds, ValDef(mod, lhs, typ, newRhs))
    
    case Block(stats, expr) => 
      val (binds, newBlock) = extractBlock(stats :+ expr)
      extractUnwrap(binds, newBlock)

    case If(cond, branch1, branch2) =>
      val (condBinds, newCond) = extractBindings(cond)
      val wrapped1 = transform(branch1)
      val wrapped2 = transform(branch2)
      extractUnwrap(condBinds, If(newCond, wrapped1, wrapped2))
      
    case Match(obj, cases) =>
      val (objBinds, newObj) = extractBindings(obj)
      val wrappedCases = cases map { case CaseDef(pat, guard, body) =>
        CaseDef(pat, guard, transform(body)) // TODO handle unwraps in guard
      }
      extractUnwrap(objBinds, Match(newObj, wrappedCases))
      
    case Annotated(ann, obj) =>
      val (objBinds, newObj) = extractBindings(obj)
      (objBinds, Annotated(ann, newObj))
      
    case Typed(obj, tpt) =>
      val (objBinds, newObj) = extractBindings(obj)
      (objBinds, Typed(newObj, tpt))
      
    case _ => {
      collectUnwrapArgs(tree) foreach { case (arg, _) =>
        c.error(arg.pos, "unwrapping is not currently supported here")
      }
      (Nil, tree)
    }
  }}
  
  /**
   * If the given tree represents an application of `unwrap`, either via the static method or the
   * postfix ops in `Unwrappable`, returns the applicand, i.e. the tree to be unwrapped.
   */
  def getUnwrapArgs(tree: Tree): Option[(Tree, Tree)] = tree match {
    
    case Apply(Apply(fun, List(arg)), List(unapply))
      if isUnwrap(fun) => Some((arg, unapply))
      
    case Select(Apply(Apply(fun, List(arg)), List(unapply)), op)
      if isMonadicToUnwrappable(fun)
      && (op == newTermName("unwrap") || op == newTermName("$bang")) => Some((arg, unapply))
        
    case _ => None
  }
  
  def extractUnwrap(tree: Tree): Option[BindGroup] = getUnwrapArgs(tree) map { case (arg, _) =>
    val (binds, newArg) = extractBindings(arg)
    extractUnwrap(binds, newArg)
  }

  /**
   * Takes a list of bindings and a monadic tree, binds the tree to a new identifier and adds
   * that binding to the list; the tree in the resulting group is just the newly-bound identifier.
   */
  def extractUnwrap(binds: List[Binding], tree: Tree): BindGroup = {
    val freshName = getFreshName()
    (binds :+ ((freshName, tree)), Ident(freshName))
  }

  def extractTraverse(tree: Tree): Option[BindGroup] = for {
    hmc <- matchHofMethodCall(tree)
    if (hmc.method == newTermName("map") || hmc.method == newTermName("flatMap") || hmc.method == newTermName("foreach"))
    if !collectUnwrapArgs(hmc.funBody).isEmpty
    (objBinds, newObj) = extractBindings(hmc.obj)
    traversed = Apply(Select(newObj, newTermName("traverse")), List(Function(List(hmc.funArg), transform(hmc.funBody))))
  } yield {
    
    def mapTraversedWith(body: TermName => Tree): Tree = {
      val v = getFreshName()
      val fParm = ValDef(Modifiers(Flag.PARAM), v, TypeTree(), EmptyTree)
      val fBody = body(v)
      Apply(Select(traversed, newTermName("map")), List(Function(List(fParm), fBody)))
    }
    
    hmc.method.encoded match {
      case "map" =>
        // `t map (x => ...)` becomes `unwrap(t traverse (x => monadically { ... }))`
        extractUnwrap(objBinds, traversed)
    
      case "flatMap" =>
        // `t flatMap (x => ...)` becomes `unwrap(t traverse (x => monadically { ... }) map (_.join))`
        extractUnwrap(objBinds, mapTraversedWith(v => Select(Ident(v), newTermName("join"))))

      case "foreach" => 
        // `t foreach (x => ...)` becomes `unwrap(t traverse (x => monadically { ... }) map (_ => ()))`
        extractUnwrap(objBinds, mapTraversedWith(_ => Literal(Constant())))
        
      // TODO handle `withFilter`
    }
  }

  // TODO make this generate guaranteed collision-free names
  def getFreshName(): TermName = newTermName(c.fresh(TMPVAR_PREFIX))
  
  /**
   * Attempt to analyze a tree into an object calling a method which takes a single HOF argument, 
   * ignoring any top-level type applications.
   */
  def matchHofMethodCall(tree: Tree): Option[HofMethodCall] = {
    def matchSelect(tree: Tree): Option[Select] = tree match {
      case sel: Select => Some(sel)
      case TypeApply(fun, _) => matchSelect(fun)
      case _ => None
    }
    tree match {
      case Apply(fun, List(Function(List(funParm), funBody))) => matchSelect(fun) map { case Select(obj, method) =>
        HofMethodCall(obj, method, funParm, funBody)
      }
      case TypeApply(fun, _) => matchHofMethodCall(fun)
      case _ => None
    }
  }
  
  case class HofMethodCall(obj: Tree, method: Name, funArg: ValDef, funBody: Tree)
  
  /**
   * Takes a list of statements, transforms them and then sequences them monadically.
   */
  def extractBlock(stmts: List[Tree]): BindGroup = stmts match {
    case expr :: Nil  => (Nil, Block(Nil, transform(expr)))
    case stmt :: rest =>
      val (bindings, newStmt) = extractBindings(stmt)
      val restGrp@(restBindings, Block(restStmts, expr)) = extractBlock(rest)
      val newBlock = 
        if (restBindings.isEmpty) Block(newStmt :: restStmts, expr)
        else Block(List(newStmt), transform(restGrp, isPure = false))
      (bindings, newBlock)
  }
  
  def resolveUnapply(tpe: Type): Option[Tree] = {
    if (tpe =:= typeOf[Nothing])
      return None
    
    val monadTyCon = typeRef(NoPrefix, typeOf[Monad[Any]].typeSymbol, Nil)
    val appliedUnapply = typeRef(NoPrefix, typeOf[Unapply[Any, Any]].typeSymbol, List(monadTyCon, tpe))
    
    val unapplyInstance = c.inferImplicitValue(appliedUnapply)
    
    if (unapplyInstance == EmptyTree)
      return None
    
    Some(unapplyInstance)
  }
  
  def getTypeClass(unapplyInstance: Tree): Tree = Select(unapplyInstance, newTermName("TC"))

  /**
   * A combination of a type constructor `M[_]` for which there exists a `Monad[M]`, 
   * along with its associated `pure` and `bind` identifiers.
   */
  case class MonadContext(tpe: Type, pure: Tree, bind: Tree)
}