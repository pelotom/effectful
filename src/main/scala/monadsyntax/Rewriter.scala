package monadsyntax

import language.experimental.macros
import language.higherKinds
import reflect.macros.Context
import scalaz.Monad
import Function.unlift

/**
 * Transforms the AST of an argument to `monadically`, rewriting `unwrap` calls
 * using `>>=` and `pure`. 
 */
private abstract class Rewriter {

  val c: Context

  import c.universe._

  val TMPVAR_PREFIX = "$tmc$"

  val mc: MonadContext = {
    val tree = c.macroApplication
    resolveMonad(tree.tpe.typeSymbol) getOrElse inferMonadOrFail(tree)
  }
  
  def inferMonadOrFail(tree: Tree): MonadContext = {
    val unwrapArgTypes = collectUnwrapArgs(tree) map (_.tpe)
    val lubType = lub(unwrapArgTypes)
    val mc = resolveMonad(lubType.typeSymbol) orElse (lubType.baseClasses collectFirst (unlift(resolveMonad(_))))
    if (!mc.isDefined) {
      val prefixMsg = "Could not infer the monadic type being used here"
      if (unwrapArgTypes.isEmpty)
        c.abort(tree.pos, s"${prefixMsg} because $UNWRAP is never used")
      val distinctTypeNames = unwrapArgTypes.map(_.typeSymbol.name.toString).distinct
      val plural = if (distinctTypeNames.size == 1) "" else "s"
      c.abort(tree.pos,
        s"Could not infer monadic supertype from $UNWRAP argument type${plural} (${distinctTypeNames.mkString(", ")})")
    }
    mc.get
  }
  
  /**
   * Collects all arguments of `unwrap` or its postfix equivalents within the given tree.
   */
  def collectUnwrapArgs(tree: Tree): List[Tree] = {
    import scala.collection.mutable.ListBuffer
    val buf: ListBuffer[Tree] = ListBuffer()
    new Traverser() {
      override def traverse(tree: Tree) = getUnwrapArg(tree) match {
        case Some(arg) => buf += arg
        case None => super.traverse(tree)
      }
    }.traverse(tree)
    buf.toList
  }

  /**
   * The entry point of the algorithm, the meat of the work is done in `transform`.
   */
  def rewrite(tree: Tree): Tree = {
    var newTree = c.resetLocalAttrs(tree)
    newTree = transform(newTree)
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
        if (isPure) Apply(mc.pure, List(tree)) // make monadic
        else tree
      case (name, unwrappedFrom) :: moreBinds => 
        val innerTree = transform((moreBinds, tree), isPure)
        // q"$unwrappedFrom.flatMap($name => $innerTree)"
        val fun = Function(List(ValDef(Modifiers(Flag.PARAM), name, TypeTree(), EmptyTree)), innerTree)
        Apply(Apply(mc.bind, List(unwrappedFrom)), List(fun))
    }
  }

  def pkg = rootMirror.staticPackage("monadsyntax").asModule.moduleClass.asType.toType
  def wrapSymbol = pkg.member(newTermName(MONADICALLY))
  def unwrapSymbol = pkg.member(newTermName(UNWRAP))
  def conversionSymbol = pkg.member(newTermName(MONADIC_TO_WRAPPABLE))
  def isWrap(tree: Tree): Boolean = tree.symbol == wrapSymbol
  def isUnwrap(tree: Tree): Boolean = tree.symbol == unwrapSymbol
  def isConversion(tree: Tree): Boolean = tree.symbol == conversionSymbol

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
      
    case _ => (Nil, tree)
  }}
  
  /**
   * If the given tree represents an application of `unwrap`, either via the static method or the
   * postfix ops in `Unwrappable`, returns the applicand, i.e. the tree to be unwrapped.
   */
  def getUnwrapArg(tree: Tree): Option[Tree] = tree match {
    
    case Apply(fun, List(arg)) 
      if isUnwrap(fun) => Some(arg)
        
    case Select(Apply(fun, List(arg)), op)
      if isConversion(fun) 
      && (op == newTermName("unwrap") || op == newTermName("$bang")) => Some(arg)
        
    case _ => None
  }

  def extractUnwrap(tree: Tree): Option[BindGroup] = getUnwrapArg(tree) map { arg =>
    val (binds, newArg) = extractBindings(arg)
    extractUnwrap(binds, newArg)
  }

  /**
   * Takes a list of bindings and a monadic tree, binds the tree to a new identifier and adds
   * that binding to the list; the tree in the resulting group is just the newly-bound identifier.
   */
  def extractUnwrap(binds: List[Binding], tree: Tree): BindGroup = {
    // TODO make this generate guaranteed collision-free names
    val freshName = newTermName(c.fresh(TMPVAR_PREFIX))
    (binds :+ ((freshName, tree)), Ident(freshName))
  }

  def extractTraverse(tree: Tree): Option[BindGroup] = for {
    hmc <- matchHofMethodCall(tree)
    if hmc.method == newTermName("map")
    if !collectUnwrapArgs(hmc.funBody).isEmpty
    (objBinds, newObj) = extractBindings(hmc.obj)
    newApp = Apply(Select(newObj, hmc.method), List(Function(List(hmc.funArg), transform(hmc.funBody))))
  } yield extractUnwrap(objBinds, Select(newApp, newTermName("sequence")))
  
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

  /**
   * Called on a symbol for a monadic type constructor `M[_]`, determines the `Monad` 
   * instance to use for it, if possible. This technique lifted from the scala-idioms
   * library.
   */
  def resolveMonad(sym: Symbol): Option[MonadContext] = {
    if (sym == typeOf[Nothing].typeSymbol)
      return None
    
    val tpe = TypeRef(NoPrefix, sym, Nil)
  
    val monadTypeRef = typeRef(NoPrefix, typeOf[Monad[Any]].typeSymbol, List(tpe))
    val monadInstance = c.inferImplicitValue(monadTypeRef)
  
    if (monadInstance == EmptyTree)
      return None

    val pure = Select(monadInstance, newTermName("pure"))
    val bind = Select(monadInstance, newTermName("bind"))

    Some(MonadContext(tpe, pure, bind))
  }

  /**
   * A combination of a type constructor `M[_]` for which there exists a `Monad[M]`, 
   * along with its associated `pure` and `bind` identifiers.
   */
  case class MonadContext(tpe: Type, pure: Tree, bind: Tree)
}