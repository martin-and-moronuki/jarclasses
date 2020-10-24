module RunHaskell where

import Haskell
import System.Environment
import System.Process
import Prelude

main :: IO ()
main = getArgs >>= \args -> callProcess "runhaskell" (["-ilib"] <> extensionFlags <> args)
