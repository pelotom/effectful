import language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.Context

package object monadsyntax {
  
  private[monadsyntax] val MONADICALLY = "monadically"
  private[monadsyntax] val UNWRAP = "unwrap"
  
  def monadically[M[_], A](expr:A): M[A] = macro monadicallyImpl[M, A]
  
  @deprecated(s"`$UNWRAP` must be enclosed in a `$MONADICALLY` block", "0.1")
  def unwrap[M[_], A](expr:M[A]):A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  def monadicallyImpl[M[_], A](c1: Context)(expr:c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new { val c: c1.type = c1 } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree))
  }
}