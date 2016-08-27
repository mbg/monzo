# Haskell bindings for the Monzo API

[![Hackage](https://img.shields.io/hackage/v/monzo.svg?style=flat)](https://hackage.haskell.org/package/monzo)
[![Build Status](https://travis-ci.org/mbg/monzo.svg?branch=master)](https://travis-ci.org/mbg/monzo)

## Getting started

There is some example code in `examples/Example.hs`. The code assumes that a file named `token.txt` containing a valid OAuth token is in the same folder.

To run the example, run `cabal repl` (if using a cabal sandbox) or `ghci Monzo.hs` (if not). Then load the example into the REPL with `:l examples/Example.hs`. Finally, invoke the example with `withMonzo token foo`.
