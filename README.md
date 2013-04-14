# Monad Syntax

Monads are sometimes referred to as "programmable semicolons", because they allow us to write imperative-looking blocks of code where the "effect" of each statement is determined by the monad in question. Monad syntax allows you to write configurably effectful programs in a more natural style.

## Motivation

In Scala we have `for`-comprehensions as an imperative-looking syntax for writing monadic code, e.g.

    for {
      x <- foo
      y <- bar(x)
      z <- baz
    } yield (y, z)

Each monadic assignment `a <- ma` _unwraps_ a value from the monad so that it can be used later in the computation. But this is a little less convenient than one might hope--frequently we would like to make use of an unwrapped monadic value without having to explicitly name it. With monad syntax we can write it _inline_, like so:

    monadically { (unwrap(bar(unwrap(foo))), unwrap(baz)) }

or with some extra sugar,

    monadically { (bar(foo!)!, baz!) }

where the postfix `!` means `unwrap`. 

### Conditionals

Writing conditional expressions in `for` comprehensions can get hairy fast:

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

With monad syntax we can write this more naturally as:

    monadically {
      if (foo!) 
        baz(bar!)!
      else 
        boz! * biz!
    }
