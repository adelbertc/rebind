# Rebind
## Overview
Rebind is a Scala port/remake of the Haskell [retry](https://hackage.haskell.org/package/retry) library. One
of the main differences is it is designed to work with `DisjunctionT`'s instead of `MonadIO` things.

## Getting Started
*0.1* is intended to be a *preview release* for those who want to play around with Rebind. Despite the
small bit of code that it is, there may well be breaking changes in the following versions.

Rebind is cross-built/published against Scala 2.10 and 2.11 with
[Scalaz](https://github.com/scalaz/scalaz) 7.1 - Scalaz is (currently) it's only dependency.

To use it in your project, add the following to your SBT build definition:

```
resolvers += "adelbertc" at "http://dl.bintray.com/adelbertc/maven"

libraryDependencies += "com.adelbertc" %% "rebind-core" % "0.1.0"
```

### Usage
Example usage can be found in the
[example](https://github.com/adelbertc/rebind/tree/master/example/src/main/scala/rebind/example) sub-project,
as well as in the [tests](https://github.com/adelbertc/rebind/tree/master/core/src/test/scala/rebind).

Because Rebind abstracts out the `F[_] : Monad` used in the `DisjunctionT`, it should (hopefully) be pretty easy
to make it work with libraries like [Doobie](https://github.com/tpolecat/doobie) or
[Dispatch](http://dispatch.databinder.net/Dispatch.html). The general idea is to get your "action" (probably
in the form of `scalaz.concurrent.Task` or `scalaz.effect.IO`) and then use `Rebind` to specify how you want to
retry in the case of failure to get a new retrying action back. The operations are stack stafe so long as `F[_]`
is - common examples are `Task` and `IO`.

## License
Code provided under the BSD-3 license available at http://opensource.org/licenses/BSD-3-Clause, as
well as in the LICENSE file. This is the same license used as the retry library.
