package object monadsyntax {
  import language.experimental.macros
  import language.implicitConversions
  import language.higherKinds
  import reflect.macros.Context
  import scalaz.{Monad, Unapply}
  
  private[monadsyntax] val MONADICALLY = "monadically"
  private[monadsyntax] val UNWRAP = "unwrap"
  private[monadsyntax] val MONADIC_TO_UNWRAPPABLE = "monadicToUnwrappable"
  
  def monadically[M[_], A](expr: A): M[A] = macro monadicallyImpl[M, A]
  
  def monadicallyImpl[M[_], A](c1: Context)(expr:c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new { val c: c1.type = c1 } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree))
  }
  
  @deprecated(s"Cannot unwrap outside of a `$MONADICALLY` block", "0.1")
  def unwrap[MA](expr: MA)(implicit u: Unapply[Monad, MA]): u.A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  implicit def monadicToUnwrappable[MA](ma: MA)(implicit u: Unapply[Monad, MA]): Unwrappable[MA, u.A] = new Unwrappable(ma)
}