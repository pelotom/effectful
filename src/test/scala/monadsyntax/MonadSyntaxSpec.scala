package monadsyntax

import language.higherKinds
import language.postfixOps

import scalaz._
import Scalaz._

import org.scalacheck.{Properties, Arbitrary}
import org.scalacheck.Prop._

object MonadSyntaxSpec extends Properties("monad-syntax") {
  
  property("(monadically . unwrap) == id") = {
    def test[M[_], A](implicit 
      m: Monad[M], 
      a: Arbitrary[M[A]]) = forAll { (ma: M[A]) => 
        ma == monadically(unwrap(ma))
      }
    // TODO make a meta-QuickCheck that can generate types!
    test[List, Int] &&
    test[List, Boolean] &&
    test[Option, Int] &&
    test[Option, Boolean]
  }
  
  property("explicit type application") = {
    def test[A](implicit 
      a: Arbitrary[A]) = forAll { (a: A) => 
        List(a) == monadically[List, A](unwrap(List(a)))
      }
      
    test[Int] &&
    test[Boolean]
  }
  
  property("empty block with explicit type params") = {
    def test[M[_], A](implicit 
      m: Monad[M], 
      a: Arbitrary[A]) = forAll { (x: A) =>
        x.pure[M] == monadically[M, A](x)
      }
    
    test[List, Int] &&
    test[List, Boolean] &&
    test[Option, Int] &&
    test[Option, Boolean]
  }
  
  property("product") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      ama: Arbitrary[M[A]], 
      amb: Arbitrary[M[B]]) = forAll { (ma: M[A], mb: M[B]) =>
        
        val value = monadically {
          (unwrap(ma), unwrap(mb))
        }
        
        val expected = for {
          a <- ma
          b <- mb
        } yield (a, b)
        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("nested blocks") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      ama: Arbitrary[M[A]], 
      amb: Arbitrary[M[B]]) = forAll { (ma: M[A], mb: M[B]) =>
        
        val value = monadically {
          val ma2 = monadically { (); unwrap(ma) }
          val mb2 = monadically { (); unwrap(mb) }
          (unwrap(ma2), unwrap(mb2))
        }
        
        val expected = for {
          a <- ma
          b <- mb
        } yield (a, b)

        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("homogeneous conditional") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      at: Arbitrary[M[Boolean]], 
      al: Arbitrary[M[A]]) = forAll { (test: M[Boolean], left: M[A], right: M[A]) =>
        
        val value = monadically {
          if (unwrap(test)) 
            unwrap(left)
          else 
            unwrap(right)
        }
        
        val expected = for {
          t <- test
          result <- if (t) left else right
        } yield result
        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("heterogeneous conditional") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      at: Arbitrary[M[Boolean]], 
      al: Arbitrary[M[A]],
      ar: Arbitrary[A]) = forAll { (test: M[Boolean], left: M[A], right: A) =>
        
        val value = monadically {
          if (unwrap(test)) 
            unwrap(left)
          else 
            right
        }
        
        val expected = for {
          t <- test
          result <- if (t) left else right.pure[M]
        } yield result
        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("one-armed conditional") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      at: Arbitrary[M[Boolean]], 
      al: Arbitrary[M[A]],
      ar: Arbitrary[A]) = forAll { (test: M[Boolean], left: M[A], result: A) =>
        
        val value = monadically {
          if (unwrap(test)) 
            unwrap(left)
          result
        }
        
        val expected = for {
          t <- test
          _ <- if (t) left else ().pure[M]
        } yield result
        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("heterogeneous else-if") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      at: Arbitrary[M[Boolean]], 
      aa: Arbitrary[A],
      ama: Arbitrary[M[A]]) = forAll { (test1: M[Boolean], choice1: M[A], test2: M[Boolean], choice2: A, choice3: M[A]) =>
        
        val value = monadically {
          if (unwrap(test1)) 
            unwrap(choice1)
          else if (unwrap(test2))
            choice2
          else unwrap(choice3)
        }
        
        val expected = for {
          t1 <- test1
          result <- if (t1) choice1 else {
            for {
              t2 <- test2
              innerResult <- if (t2) choice2.pure[M] else choice3
            } yield innerResult
          }
        } yield result
        
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
  
  property("unwrap outside conditional") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      at: Arbitrary[M[Boolean]], 
      al: Arbitrary[M[A]]) = forAll { (test: M[Boolean], left: M[A], right: M[A]) =>
      
        val value = monadically {
          unwrap { 
            if (unwrap(test)) 
              left
            else 
              right
          }
        }
      
        val expected = for {
          t <- test
          result <- if (t) left else right
        } yield result
      
        value == expected
      }
      
    test[List, Int, Char] &&
    test[Option, Boolean, Int]
  }
      
  property("simple match expression") = {
    
    def test[M[_], A](implicit 
      m: Monad[M],
      at: Arbitrary[M[List[A]]], 
      aa: Arbitrary[M[A]]) = forAll { (matchObj: M[List[A]], ifNil: M[A]) =>
        
        val value = monadically {
          unwrap(matchObj) match {
            case Nil => unwrap(ifNil)
            case a :: _ => a
          }
        }
        
        val expected = for {
          o <- matchObj
          result <- o match {
            case Nil => ifNil
            case a :: _ => a.pure[M]
          }
        } yield result
        
        value == expected
      }
      
    test[List, Int] &&
    test[Option, Boolean]
  }
  
  property("simple postfix unwraps") = {
    def test[M[_], A](implicit 
      m: Monad[M], 
      a: Arbitrary[M[A]]) = forAll { (ma: M[A]) => 
        ma == monadically(ma.unwrap) && ma == monadically(ma!)
      }
      
    test[List, Int] &&
    test[List, Boolean] &&
    test[Option, Int] &&
    test[Option, Boolean]
  }
  
  property("compound postfix unwraps") = {
    def test[M[_], A, B](implicit 
      m: Monad[M], 
      aa: Arbitrary[M[A]],
      af: Arbitrary[A => M[B]]) = forAll { (ma: M[A], f: A => M[B]) => 
        monadically(f(ma!)!) == ma.flatMap(f)
      }
      
    test[List, Int, Int] &&
    test[List, Int, Boolean] &&
    test[Option, Int, Int] &&
    test[Option, Int, Boolean]
  }
}