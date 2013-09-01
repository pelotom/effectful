package object effectful {
  import language.experimental.macros
  import language.implicitConversions
  import language.higherKinds
  import reflect.macros.Context
  import scalaz.{Monad, Unapply}
  import scala.reflect.internal.annotations.compileTimeOnly
  
  private[effectful] val EFFECTFULLY = "effectfully"
  private[effectful] val UNWRAP = "unwrap"
  private[effectful] val EFFECTFUL_TO_UNWRAPPABLE = "effectfulToUnwrappable"
  
  def effectfully[M[_], A](expr: A): M[A] = macro effectfullyImpl[M, A]
  
  def effectfullyImpl[M[_], A](c1: Context)(expr:c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new { val c: c1.type = c1 } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree))
  }
  
  @compileTimeOnly(s"Cannot unwrap outside of a `$EFFECTFULLY` block")
  def unwrap[MA](expr: MA)(implicit u: Unapply[Monad, MA]): u.A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  implicit def effectfulToUnwrappable[MA](ma: MA)(implicit u: Unapply[Monad, MA]): Unwrappable[MA, u.A] = new Unwrappable(ma)
}