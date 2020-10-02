#! /usr/bin/env runhaskell

import System.Environment
import System.Process

main :: IO ()
main = getArgs >>= \args -> callProcess "runhaskell" (["-ilib", "-XBlockArguments", "-XConstraintKinds", "-XLambdaCase", "-XNoImplicitPrelude", "-XOverloadedStrings", "-XQuasiQuotes", "-XScopedTypeVariables", "-XViewPatterns"] <> args)
