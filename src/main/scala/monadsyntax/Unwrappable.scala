package monadsyntax

import scala.language.higherKinds
import scalaz.Monad

class Unwrappable[M[_], A](ma: M[A]) {
  
  @deprecated(s"Cannot unwrap outside of a `$MONADICALLY` block", "0.1")
  def unwrap: A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  @deprecated(s"Cannot unwrap outside of a `$MONADICALLY` block", "0.1")
  def ! : A = sys.error(s"$UNWRAP was not macro'ed away!")
}