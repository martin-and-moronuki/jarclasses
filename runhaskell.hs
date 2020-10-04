#! /usr/bin/env runhaskell

import Haskell
import System.Environment
import System.Process

main :: IO ()
main = getArgs >>= \args -> callProcess "runhaskell" (["-ilib"] <> extensionFlags <> args)
