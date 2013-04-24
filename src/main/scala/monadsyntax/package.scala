package object monadsyntax {
  import language.experimental.macros
  import language.implicitConversions
  import language.higherKinds
  import reflect.macros.Context
  import scalaz.Monad
  
  private[monadsyntax] val MONADICALLY = "monadically"
  private[monadsyntax] val UNWRAP = "unwrap"
  private[monadsyntax] val MONADIC_TO_UNWRAPPABLE = "monadicToUnwrappable"
  
  def monadically[M[_], A](expr:A): M[A] = macro monadicallyImpl[M, A]
  
  @deprecated(s"Cannot unwrap outside of a `$MONADICALLY` block", "0.1")
  def unwrap[M[_], A](expr:M[A]):A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  def monadicallyImpl[M[_], A](c1: Context)(expr:c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new { val c: c1.type = c1 } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree))
  }
  
  implicit def monadicToUnwrappable[M[_], A](ma: M[A]): Unwrappable[M, A] = new Unwrappable(ma)
}