package effectful

import scala.language.higherKinds
import scalaz.Monad
import scala.reflect.internal.annotations.compileTimeOnly

class Unwrappable[MA, A](ma: MA) {
  
  @compileTimeOnly(s"Cannot unwrap outside of a `$EFFECTFULLY` block")
  def unwrap: A = sys.error(s"$UNWRAP was not macro'ed away!")
  
  @compileTimeOnly(s"Cannot unwrap outside of a `$EFFECTFULLY` block")
  def ! : A = sys.error(s"$UNWRAP was not macro'ed away!")
}