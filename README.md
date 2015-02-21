# Rebind (WIP)
This is still a work in progress!

## Overview
Rebind is a Scala port/remake of the Haskell [retry](https://hackage.haskell.org/package/retry) library. One
of the main differences is it is designed to work with `DisjunctionT`'s instead of `MonadIO` things.

## License
Code provided under the BSD-3 license available at http://opensource.org/licenses/BSD-3-Clause, as
well as in the LICENSE file. This is the same license used as the retry library.

## TODO
* Fix bug where retry combinators perform action once more than it's supposed to
* Reader for passing in RetryPolicy
* Syntax
