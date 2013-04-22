# Monad Syntax
---

Monad syntax is a small macro library that allows you to write monadic code in a more natural style, by embedding effectful expressions in other effectful expressions rather than explicitly naming intermediary results. The idea is similar to that of the [Scala Async](https://github.com/scala/async) library, but generalized to arbitrary monads (not just `Future`).

## Quick start

This library requires Scala version 2.10.0.

    git clone https://github.com/pelotom/monad-syntax.git
    cd monad-syntax
    sbt clean compile test

You can run `sbt package`, which will create a jar file in `target/scala-2.10/`; add this to your classpath and you're ready to go. Alternately, you can run `sbt console` to fire up a Scala REPL with the necessary dependencies loaded.

Write some code using monad syntax:

```scala
import scalaz._
import Scalaz._
import monadsyntax._
import language.postfixOps

val xs = List(1,2,3)
val ys = List(true,false)

val foo = monadically { (xs!, ys!) }

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

or using the postfix `unwrap` operator (`!`), simply

```scala
monadically { (bar(foo!)!, baz!) }
```

### Conditionals

Writing conditional expressions in `for` comprehensions can get hairy fast:

```scala
for {
  x <- foo
  result <- if (x > 12) {
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
  if (foo.! > 12)
    baz(bar!)!
  else 
    (boz.! * biz.!)
}
```

Effectful `match`/`case` expressions are similarly easier to express with monad syntax.

## How it works
    
`monadically` demarcates a block of code in which monad syntax will be used. Each invocation of `unwrap` / `!` that occurs within such a block seems to take a monadic value of type `M[A]` and return a pure value of type `A`. Of course that's not generally possible with most monads, so something magical must be going on... and in fact it is. Just as `for`-comprehensions transform your code into `flatMap`s and `map`s behind the scenes, `monadically` is a macro which transforms code using `unwrap` into calls to `bind` and `pure` from Scalaz's `Monad` type class. So monad syntax only works with instances of `Monad`.

Why require `Monad` instead of just using `flatMap`s and `map`s? Unfortunately, `pure` is necessary in order to get certain things to work. In particular, a conditional in which one branch contains calls to `unwrap` but the other doesn't necessitates the use of `pure`.

## Limitations

Within the lexical scope of a `monadically` block, not all invocations of `unwrap` / `!` are valid; in particular function definitions cannot contain `unwrap` calls currently. Future work will go to supporting `unwrap` within anonymous functions under certain circumstances. When `unwrap` is used in an illegal position, it will be flagged with a deprecation warning (in Scala 2.10.2 this will be an error).
