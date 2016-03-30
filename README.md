# Haskell bindings for the Mondo API

## Getting started

There is some example code in `examples/Example.hs`. The code assumes that a file named `token.txt` containing a valid OAuth token is in the same folder.

To run the example, run `cabal repl` (if using a cabal sandbox) or `ghci Mondo.hs` (if not). Then load the example into the REPL with `:l examples/Example.hs`. Finally, invoke the example with `withMondo token foo`.
