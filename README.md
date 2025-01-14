espresso - Purely Functional Expression DSL Builder Library
===========================================================

For building DSLs (Domain-Specific Languages) that allow the composition of pure and fully type-safe expressions such
that any sub-expression may generate or receive errors as input, short-circuiting evaluation.  Such DSLs include support for readers that can read for weakly-typed (i.e. "unsafe") data sources.

Ordering an espresso
--------------------

```
resolvers ++= Seq(
    "jitpack" at "https://jitpack.io"
)

libraryDependencies ++= Seq(
    "com.github.ryanonsrc" %% "espresso" % "v1.1.1"
)
```

espresso makes extensive use of the Typelevel cats and shapeless libraries as well as the kind-projector
compiler plugin.

A simple example of an expression syntax and library
----------------------------------------------------

Take a look at [https://github.com/ryanonsrc/espresso/tree/master/src/test/scala/io/nary/espresso/sample](https://github.com/ryanonsrc/espresso/tree/master/src/test/scala/io/nary/espresso/sample)

Fundamental combinators (i.e. units of composition)
---------------------------------------------------

* `Term[E, A]` - represents a **term** within an expression, evaluating to an error of type `E` or result of type `A`
* `Expr[E, A, B]` - for any expression taking a value of type `A` and evaluating to either a result of type `B` or an error of type `E`
* `In[E, A]` - weakly-typed `Any` input from "the outside world" that can be "read" into an expression as either value of type `A`or an error of type`E`
* `opN[E, A1, ... AN, B]` - wraps an operation: `(Term[E, A1],...,Term[E, AN]) => (Term[E, B])` produces an `Expr[E, (A1,...,AN), B]`
* `joinN[E, A, B1, ... BN]` - combine an tuple of `N` expressions: `Expr[E, A, Bi]` into a single `Expr[E, A, (B1,...,BN)]`
* `evalN[E, A, B1, ... BN, C]` - Given `N` expressions: `Expr[E, A, Bi]` and operation: `Expr[E, (B1,...,BN), C]`, generate `Expr[E, A, C]`

## _`lift`ing_ functions and values into `Expr`essions

* `const[E, A, B]`- _lift_ a value of type`B`directly into an expression (bypassing the need to _read_ from a weakly-typed source`A`)
* `error[E, A, B]` - _lift_ an error instance of type `E` directly into an expression (bypassing the need to _read_ from a weakly-typed source `A`)
* `funcExprN[E, A1,... AN, B]` - _lift_ functions of type `(A1, ..., AN) => B` `String => E` (for errors) into the expression: `Expr[E, (A1,...,AN), B]`

## Visualizing Expression Composition

<img src"./Visualize-Composition.png" width="50%">

## Quick-Start Guide for building a DSL

create any implicit readers/conversions needed for "reading" values from Source using the `espresso.adapters.reader`:

1. ```
   def read[E, A, K, S](k: K, orElse: E, 
                        f: S => K => In[E, A] => Option[Term[E, A]])
                       (using i: In[E, A]): Expr[E, S, A]
   ```
2. define your _library_ of `Expr[E, A, B]` operations either using `opN`-wrappers or lifted-`funcExprN`s
3. create your DSL syntax in the form of an `extension` of l-`Expr[<Your DSL Error type>, <Your DSL expression data source type>, <Your DSL produced value type>]` as functions that will evaluate the l-`Expr` with a provided library function using an `evalN` combinator
