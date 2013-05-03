package effectful

import scalaz._

object TestCustomTraverseType {
  sealed trait MyList[+A]
  case class MyNil[A]() extends MyList[A]
  case class MyCons[+A](x: A, xs: MyList[A]) extends MyList[A]

  implicit object myListTraverse extends Traverse[MyList] with MonadPlus[MyList] {
    
    def traverseImpl[G[_]: Applicative, A, B](fa: MyList[A])(f: A => G[B]): G[MyList[B]] = {
      val app = implicitly[Applicative[G]]
      fa match {
        case MyNil() => app.point(MyNil[B]())
        case MyCons(x, xs) => app.ap(traverseImpl(xs)(f))(app.ap(f(x))(app.point((x: B) => (xs: MyList[B]) => (MyCons(x, xs):MyList[B]))))
      }
    }
    
    def point[A](a: => A): MyList[A] = MyCons(a, MyNil())
    
    def bind[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = fa match {
      case MyNil() => MyNil()
      case MyCons(x, xs) => plus(f(x), bind(xs)(f))
    }
    
    def plus[A](a: MyList[A], b: => MyList[A]): MyList[A] = a match {
      case MyNil() => b
      case MyCons(x, xs) => MyCons(x, plus(xs, b))
    }
    
    def empty[A]: MyList[A] = MyNil()
  }
}