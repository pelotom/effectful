package effectful

import scala.language.experimental.macros
import scala.reflect.macros.{Context, TypecheckException}
import scala.reflect.ClassTag

import scalaz._
import Scalaz._

import Function.unlift
import scala.collection.generic.FilterMonadic

/**
 * Transforms the AST of an argument to `effectfully`, rewriting `unwrap` calls
 * using `>>=` and `pure`. 
 */
private abstract class Rewriter {

  val c: Context

  import c.universe._

  val TMPVAR_PREFIX = "$tmc$"

  val unapplyInstance: Tree = {
    val tree = c.macroApplication
    resolveUnapply[Monad[Any]](tree.tpe) getOrElse inferUnapplyOrFail(tree)
  }
  
  val unapplyName = newTermName(TMPVAR_PREFIX + "unapply")
  
  val monadInstance = getUnapplyTC(Ident(unapplyName))
  
  def inferUnapplyOrFail(tree: Tree): Tree = {
    val unapplies = collectUnwrapArgs(tree) map (_._2)
    
    if (unapplies.isEmpty)
      c.abort(tree.pos, s"could not infer the monad in question because $UNWRAP is never used")
      
    val instanceTypes = groupWhen(unapplies.map(u => c.typeCheck(getUnapplyTC(u)).tpe))(_=:=_).map(_(0))
    if (instanceTypes.size > 1)
      c.abort(tree.pos, s"cannot unwrap more than one monadic type in a given $EFFECTFULLY block")

    unapplies(0)
  }
  
  def getUnapplyTC(unapplyInstance: Tree): Tree = Select(unapplyInstance, newTermName("TC"))
  
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
          && !isEffectfulToUnwrappable(fun) => transform(fun)
        case Apply(fun, List(arg))
          if isImplicitConversion
          && !isEffectfulToUnwrappable(fun) => transform(arg)
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
   * For relevant trees find type classes their types belong to and attach them for
   * use during the transformation phase.
   */
  def saveOldTree(tree: Tree): Tree = {
    new Traverser() {
      override def traverse(tree: Tree) {
        tree.updateAttachment(OldTree(tree))
        super.traverse(tree)
      }
    }.traverse(tree)
    c.resetLocalAttrs(tree)
  }
  
  /**
   * Borrowed from the Scala Async library: copy position & attachments from one
   * tree to another.
   */
  def attachCopy[T <: Tree](orig: Tree, tree: T): tree.type = {
    tree.setPos(orig.pos)
    for (att <- orig.attachments.all)
      tree.updateAttachment[Any](att)(ClassTag.apply[Any](att.getClass))
    tree
  }
  
  case class OldTree(tree: Tree)

  /**
   * The entry point of the algorithm, the meat of the work is done in `transform`.
   */
  def rewrite(tree: Tree): Tree = {
    var newTree = tree
    try {
      newTree = stripImplicits(newTree)
      newTree = saveOldTree(newTree)
      newTree = transform(newTree)
      newTree = addUnapplyToScope(newTree)
    } catch {
      case e: TypecheckException => return tree
    }
    
    try {
      newTree = c.typeCheck(newTree)//, WildcardType, true)
    } catch {
      case e: TypecheckException =>
        c.abort(e.pos.asInstanceOf[Position], e.msg)
        return tree
    }
    
    val TypeRef(pre, sym, args) = c.macroApplication.tpe
    if (sym == typeOf[Nothing].typeSymbol)
      // fix up type inference
      c.macroApplication.setType(newTree.tpe)
    newTree
  }

  /**
   * Takes a tree representing an expression of type `A`, possibly containing `unwrap` calls,
   * and transforms it into a tree representing an expression of type `M[A]`, using effectful
   * operations.
   */
  def transform(tree: Tree): Tree = transform(extractBindings(tree))

  def transform(group: BindGroup, isPure: Boolean = true): Tree = group match { case(binds, tree) =>
    binds match {
      case Nil => 
        if (isPure) Apply(Select(monadInstance, newTermName("pure")), List(tree)) // make effectful
        else tree
      case (name, unwrappedFrom) :: moreBinds => 
        val innerTree = transform((moreBinds, tree), isPure)
        // q"$unwrappedFrom.flatMap($name => $innerTree)"
        val fun = Function(List(ValDef(Modifiers(Flag.PARAM), name, TypeTree(), EmptyTree)), innerTree)
        Apply(Apply(Select(monadInstance, newTermName("bind")), List(unwrappedFrom)), List(fun))
    }
  }

  def pkg = rootMirror.staticPackage("effectful").asModule.moduleClass.asType.toType
  def wrapSymbol = pkg.member(newTermName(EFFECTFULLY))
  def unwrapSymbol = pkg.member(newTermName(UNWRAP))
  def conversionSymbol = pkg.member(newTermName(EFFECTFUL_TO_UNWRAPPABLE))
  def isWrap(tree: Tree): Boolean = tree.symbol == wrapSymbol
  def isUnwrap(tree: Tree): Boolean = tree.symbol == unwrapSymbol
  def isEffectfulToUnwrappable(tree: Tree): Boolean = tree.symbol == conversionSymbol

  type Binding = (TermName, Tree)

  /**
   * A `BindGroup` represents a sequence of effectful bindings and the Tree in which they
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
  def extractBindings(tree: Tree): BindGroup = {
    val (binds, newTree) = extractUnwrap(tree) orElse extractHofCall(tree) getOrElse { tree match {
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
        val wrappedCases = cases map { case cas@CaseDef(pat, guard, body) =>
          attachCopy(cas, CaseDef(pat, guard, transform(body))) // TODO handle unwraps in guard
        }
        extractUnwrap(objBinds, Match(newObj, wrappedCases))
      
      case Annotated(ann, obj) =>
        val (objBinds, newObj) = extractBindings(obj)
        (objBinds, Annotated(ann, newObj))
      
      case Typed(obj, _) => extractBindings(obj)
      
      case _ => {
        collectUnwrapArgs(tree) foreach { case (arg, _) =>
          c.error(arg.pos, "unwrapping is not currently supported here")
        }
        (Nil, tree)
      }
    }}
    (binds, attachCopy(tree, newTree))
  }
  
  /**
   * If the given tree represents an application of `unwrap`, either via the static method or the
   * postfix ops in `Unwrappable`, returns the applicand, i.e. the tree to be unwrapped.
   */
  def getUnwrapArgs(tree: Tree): Option[(Tree, Tree)] = tree match {
    
    case Apply(Apply(fun, List(arg)), List(unapply))
      if isUnwrap(fun) => Some((arg, unapply))
      
    case Select(Apply(Apply(fun, List(arg)), List(unapply)), op)
      if isEffectfulToUnwrappable(fun)
      && (op == newTermName("unwrap") || op == newTermName("$bang")) => Some((arg, unapply))
        
    case _ => None
  }
  
  def extractUnwrap(tree: Tree): Option[BindGroup] = getUnwrapArgs(tree) map { case (arg, _) =>
    val (binds, newArg) = extractBindings(arg)
    extractUnwrap(binds, newArg)
  }

  /**
   * Takes a list of bindings and a effectful tree, binds the tree to a new identifier and adds
   * that binding to the list; the tree in the resulting group is just the newly-bound identifier.
   */
  def extractUnwrap(binds: List[Binding], tree: Tree): BindGroup = {
    val freshName = getFreshName()
    (binds :+ ((freshName, tree)), Ident(freshName))
  }

  /**
   * If the given tree is an invocation of one of the supported higher-order methods below, applied 
   * to an anonymous function, transform the function body according to the following plan:
   *  - `t map (x => ...)` becomes `unwrap(t traverse (x => effectfully { ... }))` 
   *  - `t flatMap (x => ...)` becomes `unwrap(t traverse (x => effectfully { ... }) map (_.join))`
   *  - `t foreach (x => ...)` becomes `unwrap(t traverse (x => effectfully { ... }) map (_ => ()))`
   *  - `t withFilter {x => ...}` becomes `unwrap(t filterM (x => effectfully { ... }))`
   */
  def extractHofCall(tree: Tree): Option[BindGroup] = {
  
    case class HofCall(obj: Tree, method: String, funArg: ValDef, funBody: Tree)
  
    /**
     * Attempt to analyze a tree into an object calling a method which takes a single HOF argument, 
     * ignoring any top-level type applications.
     */
    def matchHofCall(tree: Tree): Option[HofCall] = {
      def matchSelect(tree: Tree): Option[Select] = tree match {
        case sel: Select => Some(sel)
        case TypeApply(fun, _) => matchSelect(fun)
        case _ => None
      }
      tree match {
        case Apply(fun, List(Function(List(funParm), funBody))) => matchSelect(fun) map { case Select(obj, method) =>
          HofCall(obj, method.encoded, funParm, funBody)
        }
        case TypeApply(fun, _) => matchHofCall(fun)
        case _ => None
      }
    }
    
    def mkHof(obj: Tree, method: String, fun: Function): Tree = Apply(Select(obj, newTermName(method)), List(fun))

    def mkFun(fn: Ident => Tree): Function = {
      val v = getFreshName()
      val fParm = ValDef(Modifiers(Flag.PARAM), v, TypeTree(), EmptyTree)
      val fBody = fn(Ident(v))
      Function(List(fParm), fBody)
    }

    def mkMethodCall(obj: Tree, method: String, arg: Tree) = {
      Apply(Select(obj, newTermName(method)), List(arg))
    }
    
    /**
     * If the given tree is a pure `withFilter` call, change it to `filter`. This is to avoid problems due
     * to the fact that the `FilterMonadic` return type of `withFilter` is not an instance of `Traverse`.
     */
    def fixFilter(tree: Tree): Tree = {
      val changed = for (hmc <- matchHofCall(tree); if hmc.method == "withFilter")
        yield attachCopy(tree, mkHof(hmc.obj, "filter", Function(List(hmc.funArg), hmc.funBody)))
      changed getOrElse tree
    }
    
    for {
      hmc <- matchHofCall(tree)
      if List("map", "flatMap", "foreach", "withFilter") contains hmc.method // these are the only HOFs we can transform
      if !collectUnwrapArgs(hmc.funBody).isEmpty // the HOF is effectful
      (objBinds, newObj1) = extractBindings(hmc.obj)
      newObj = fixFilter(newObj1)
    } yield {
      
      val newHofArg = Function(List(hmc.funArg), transform(hmc.funBody))
      
      lazy val traverse = resolveInstanceOrFail[Traverse[Any]](newObj)
      lazy val travMonad = resolveInstanceOrFail[Monad[Any]](newObj)
      
      lazy val traversed = Apply(mkMethodCall(mkMethodCall(traverse, "traversal", monadInstance), "run", newObj), List(newHofArg))
      lazy val mapped = mkMethodCall(monadInstance, "map", traversed)
      
      val hof = hmc.method match {
        case "map" =>         traversed
        case "flatMap" =>     Apply(mapped, List(mkFun { v => mkMethodCall(travMonad, "join", v) }))
        case "foreach" =>     Apply(mapped, List(mkFun { _ => Literal(Constant(())) }))
        case "withFilter" =>  mkHof(newObj, "filterM", newHofArg)
      }
    
      extractUnwrap(objBinds, hof)
    }
  }
  
  // TODO make this generate guaranteed collision-free names
  def getFreshName(): TermName = newTermName(c.fresh(TMPVAR_PREFIX))
  
  /**
   * Takes a list of statements, transforms them and then sequences them effectfully.
   */
  def extractBlock(stmts: List[Tree]): BindGroup = stmts match {
    case expr :: Nil  => (Nil, Block(Nil, transform(expr)))
    case stmt :: rest =>
      val (bindings, newTree) = extractBindings(stmt)
      // The newTree might not actually be a statement but just a standalone identifier,
      // and as of 2.10.2 "pure" expressions in statement position are considered an error,
      // so we excise them here.
      var newStmt = List(newTree) filter ({
        case Ident(_) => false
        case _ => true
      })
      val restGrp@(restBindings, Block(restStmts, expr)) = extractBlock(rest)
      val newBlock = 
        if (restBindings.isEmpty) Block(newStmt ++ restStmts, expr)
        else Block(newStmt, transform(restGrp, isPure = false))
      (bindings, newBlock)
  }
  
  def resolveUnapply[TC : TypeTag](tpe: Type): Option[Tree] = {
    if (tpe =:= typeOf[Nothing])
      return None
    
    val unappliedTc = typeRef(NoPrefix, typeOf[TC].typeSymbol, Nil)
    val appliedUnapply = typeRef(NoPrefix, typeOf[Unapply[Any, Any]].typeSymbol, List(unappliedTc, tpe))
    
    val unapplyInstance = c.inferImplicitValue(appliedUnapply)
    
    if (unapplyInstance == EmptyTree)
      return None
    
    Some(unapplyInstance)
  }
  
  /**
   * Like `resolveInstance`, but fails with a type error instead of returning `None`. Also
   * acts on a term `Tree` and uses the type of its attachment.
   */
  def resolveInstanceOrFail[TC : TypeTag](tree: Tree): Tree = {
    val oldTree = tree.attachments.get[OldTree]
    if (!oldTree.isDefined)
      c.abort(tree.pos, s"no type information could be found for $tree")
    
    var tpe = oldTree.get.tree.tpe.widen
    
    // the following bit of hairiness is because when we use filtered for-comprehensions,
    // the type becomes `FilterMonadic[A, B]`, but what we're really dealing with is `B`,
    // because we either changed the `withFilter` call to `filter` if it was pure, or
    // to `filterM` if it was effectful.
    tpe = if (tpe.typeSymbol == typeOf[FilterMonadic[Any, Any]].typeSymbol) {
      val TypeRef(_, _/*FilterMonadic*/, List(_, realTpe)) = tpe
      realTpe
    } else tpe
    
    val instance = resolveInstance[TC](tpe)
    if (!instance.isDefined)
      c.abort(tree.pos, s"no implicit ${typeOf[TC].typeSymbol.name} instance found for ${tpe.typeSymbol.name}")
    instance.get
  }

  /**
   * Resolve a type class instance for a given type constructor. The `TC` type argument is the
   * type class' type constructor applied to some type (so that it can have a type tag), e.g.
   * `Monad[Any]`. The `tpe` argument is the tree for the type constructor for which an instance
   * of the class is being sought.
   *
   * This algorithm searches through the linearized supertypes of the `tpe` argument looking
   * for an implicit type class instance for each type. It performs better than `resolveUnapply`
   * in some respects, and worse in others. It is better in that it will find, e.g. a `Monad`
   * instance for `Some`, but worse in that it will not find an instance for, e.g. `State[A, B]`
   * because it it is only looking at type constructors of kind `* -> *`. This approach seems
   * pretty good for looking up `Traverse` instances and `Monad` instances for traversable
   * types. At some point it might be good to combine the two approaches into one uber type class
   * inference algorithm that can do everything well.
   */
  def resolveInstance[TC : TypeTag](tpe: Type): Option[Tree] = {
    def resolve(pre: Type)(sym: Symbol): Option[Tree] = {
      if (sym == typeOf[Nothing].typeSymbol)
        return None
      
      val tyCon = TypeRef(pre, sym, Nil)
      
      val reAppliedTc = typeRef(NoPrefix, typeOf[TC].typeSymbol, List(tyCon))
      val tcInstance = c.inferImplicitValue(reAppliedTc)
      
      if (tcInstance == EmptyTree)
        return None
    
      Some(tcInstance)
    }
    
    val (pre, sym) = tpe match {
      case TypeRef(pre, sym, _) => (pre, sym)
      case _ => (NoPrefix, tpe.typeSymbol)
    }
    
    val instances = (tpe.typeSymbol :: tpe.baseClasses).toStream collect unlift(resolve(pre) _)
    
    return if (instances.isEmpty) None else Option(instances.head)
  }
}