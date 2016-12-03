espresso - Purely Functional Expression DSL Builder Library
======

For building DSLs that allow the composition of pure and fully type-safe expressions such
that any sub-expression may generate or receive errors as input, short-circuiting evaluation.

Ordering an espresso
------

```
resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
    "io.nary" %% "espresso" % "0.0.1"
)
```

espresso makes extensive use of the Typelevel cats and shapeless libraries as well as the kind-projector
compiler plugin.

Sample expression syntax and library
------

Take a look at <https://github.com/ryanonsrc/espresso/tree/master/src/test/scala/io/nary/espresso/sample>

Fundamental units of composition
-----

* `Term[E, A]` - Represents a **term** within an expression, evaluating to an error of type `E` or result of type `A`
* `Expr[E, A, B]` - For any expression taking a value of type `A` and evaluating to either a result of type `B` or an error of type `E`
* `In[E, A]` - Input that can be "read" into an expression as either value of type `A` or an error of type `E`
* `opN` - Lift a function f: (HList of `N` terms ⇒ `Term[E, B]`) into a `Expr[E, A1 :: ... :: AN :: HNil, B]`
* `joinN` - Combine an Hlist of `N` expressions: `Expr[E, A, Bi]` into a single `Expr[E, A, B1 :: ... :: BN :: HNil]`
* `evalN` - Given `N` expressions: `Expr[E, A, Bi]` and operation: `Expr[E, B1 :: ... :: BN :: HNil, C]`, generate `Expr[E, A, C]`
