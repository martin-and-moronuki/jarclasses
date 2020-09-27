module Logging where

import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Relude

type LogHandle = STM.TChan String

withLog :: (LogHandle -> IO a) -> IO a
withLog go =
  atomically STM.newTChan >>= \l ->
    withLogPrinting l (go l)

writeToLog :: LogHandle -> String -> IO ()
writeToLog l s = atomically (STM.writeTChan l s)

withLogPrinting :: LogHandle -> IO a -> IO a
withLogPrinting l go = withAsync (printLogs l) \_ -> go

printLogs :: LogHandle -> IO a
printLogs l = forever printOne
  where
    printOne = pop >>= putStrLn
    pop = atomically $ STM.readTChan l
