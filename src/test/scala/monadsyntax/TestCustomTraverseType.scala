package monadsyntax

import scalaz.{Traverse, Applicative}

object TestCustomTraverseType {
  sealed trait MyList[+A]
  case class MyNil[A]() extends MyList[A]
  case class MyCons[+A](x: A, xs: MyList[A]) extends MyList[A]

  implicit object myListInstance extends Traverse[MyList] {
    def traverseImpl[G[_]: Applicative, A, B](fa: MyList[A])(f: A => G[B]): G[MyList[B]] = {
      val app = implicitly[Applicative[G]]
      fa match {
        case MyNil() => app.point(MyNil[B]())
        case MyCons(x, xs) => app.ap(traverseImpl(xs)(f))(app.ap(f(x))(app.point((x: B) => (xs: MyList[B]) => (MyCons(x, xs):MyList[B]))))
      }
    }
  }
}