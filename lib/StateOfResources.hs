module StateOfResources where

import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe
import Relude
import qualified StmContainers.Map as STM.Map

data ResourceStatus = Building | Built deriving (Eq, Ord)

type StateOfResources r = STM.Map.Map r ResourceStatus

type IsResource r = (Eq r, Hashable r)

ensureResourceBuilt :: IsResource r => IO () -> StateOfResources r -> r -> IO ()
ensureResourceBuilt buildResource rs r = bracketOnError lock (const clear) go
  where
    lock :: IO Bool =
      atomically $
        STM.Map.lookup r rs >>= \case
          Nothing -> lockResourceBuilding rs r *> pure True
          Just Built -> pure False
          Just Building -> STM.retry
    clear :: IO () = atomically (clearResourceStatus rs r)
    go :: Bool -> IO () = \case
      False -> pure ()
      True -> buildResource *> atomically (recordResourceBuilt rs r)

clearResourceStatus :: IsResource r => StateOfResources r -> r -> STM ()
clearResourceStatus rs r = STM.Map.delete r rs

recordResourceBuilt :: IsResource r => StateOfResources r -> r -> STM ()
recordResourceBuilt rs r = STM.Map.insert Built r rs

lockResourceBuilding :: IsResource r => StateOfResources r -> r -> STM ()
lockResourceBuilding rs r = STM.Map.insert Building r rs
