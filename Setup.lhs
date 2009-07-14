#!/usr/bin/env runhaskell

> import Distribution.Simple
>
> import System.Cmd (system)
> import System.Exit (exitWith)
>
> main = defaultMainWithHooks $ simpleUserHooks { runTests = check }
>     where
>       check _ _ _ _ = system "make -s check" >>= exitWith
