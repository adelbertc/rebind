# Rebind
## Overview
Rebind is a Scala port/remake of the Haskell [retry](https://hackage.haskell.org/package/retry) library. One
of the main differences is it is designed to work with `DisjunctionT`'s instead of `MonadIO` things.

### Usage
Example usage can be found in the
[example](https://github.com/adelbertc/rebind/tree/master/example/src/main/scala/rebind/example) sub-project,
as well as in the [tests](https://github.com/adelbertc/rebind/tree/master/core/src/test/scala/rebind).

## License
Code provided under the BSD-3 license available at http://opensource.org/licenses/BSD-3-Clause, as
well as in the LICENSE file. This is the same license used as the retry library.
