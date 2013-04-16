# Monad Syntax
---

Monad syntax is a small macro library that allows you to write monadic code in a more natural style, by embedding effectful expressions in other effectful expressions rather than explicitly naming intermediary results. The idea is similar to that of the [Scala Async](https://github.com/scala/async) library, but generalized to arbitrary monads (not just `Future`).

## Quick start

```scala
import scalaz._
import Scalaz._
import monadsyntax.{monadically, unwrap => choose}

val xs = List(1,2,3)
val ys = List(true,false)

val foo = monadically { (choose(xs), choose(ys)) }

// foo == List((1,true), (1,false), (2,true), (2,false), (3,true), (3,false))
```

## Motivation

In Scala we have `for`-comprehensions as an imperative-looking syntax for writing monadic code, e.g.

```scala
for {
  x <- foo
  y <- bar(x)
  z <- baz
} yield (y, z)
```

Each monadic assignment `a <- ma` _unwraps_ a pure value `a: A` from a monadic value `ma: M[A]` so that it can be used later in the computation. But this is a little less convenient than one might hope--frequently we would like to make use of an unwrapped value without having to explicitly name it. With monad syntax we can write it _inline_, like so:

```scala
monadically { (unwrap(bar(unwrap(foo))), unwrap(baz)) }
```

### Conditionals

Writing conditional expressions in `for` comprehensions can get hairy fast:

```scala
for {
  x <- foo
  result <- if (x) {
    for {
      a1 <- bar
      a2 <- baz(a1)
    } yield a2
  } else {
    for {
      b1 <- boz
      b2 <- biz
    } yield b1 * b2
  }
} yield result
```

With monad syntax we can write this as:

```scala
monadically {
  if (unwrap(foo)) 
    unwrap(baz(unwrap(bar)))
  else 
    unwrap(unwrap(boz) * unwrap(biz))
}
```

## How it works
    
`monadically` demarcates a block of code in which monad syntax will be used. Each invocation of `unwrap` that occurs within such a block seems to take a monadic value of type `M[A]` and return a pure value of type `A`. Of course that's not generally possible with most monads, so something magical must be going on... and in fact it is. Just as `for`-comprehensions transform your code into `flatMap`s and `map`s behind the scenes, `monadically` is a macro which transforms code using `unwrap` into calls to `bind` and `pure` from Scalaz's `Monad` type class. So monad syntax only works with instances of `Monad`.

Why require `Monad` instead of just using `flatMap`s and `map`s? Unfortunately, `pure` is necessary in order to get certain things to work. In particular, a conditional in which one branch contains calls to `unwrap` but the other doesn't necessitates the use of `pure`.

Scalaz is a fairly heavyweight dependency just to get a `Monad` type class, so that may change in the future.
