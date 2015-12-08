## cached-io: cache a single IO action

Sometimes you have an action that does something really expensive
whose results don't change that much. This is a simple library that
lets you cache the output of that expensive action for a
developer-specified length of time.

This library is slightly more powerful than
[io-memoize](https://hackage.haskell.org/package/io-memoize) which
lets you memoize the action forever.

### Example

See test/test-cachedIO.hs.

#### Test

Testing Jenkins stuff.
