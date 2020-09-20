#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (bracketOnError, finally)
import Control.Monad (forever)
import qualified Data.Map as Map
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Path
import Path (Abs, Dir, File, Path, Rel)
import Relude
import qualified System.FSNotify as FSN

main :: IO ()
main =
  atomically STM.newTChan >>= \l ->
    atomically (STM.newTVar mempty) >>= \rs ->
      withLogPrinting l $
        FSN.withManager \man ->
          withWatches man (react l rs) ["menus", "posts"] $
            serve l rs

react :: LogHandle -> ResourcesState -> FSN.Event -> IO ()
react l _rs e = writeToLog l (show e)

---  dev web server  ---

serve :: LogHandle -> ResourcesState -> IO ()
serve l rs = Warp.runEnv 8000 (webapp l rs)

webapp :: LogHandle -> ResourcesState -> WAI.Application
webapp l rs request respond = ensureResourceBuilt l rs r *> go
  where
    r :: Resource = WAI.pathInfo request
    go = case (responseResource r) of
      Just response -> respond response
      Nothing -> undefined

responseResource :: Resource -> Maybe Response
responseResource r = resourceFilePath r >>= \fp -> pure $ WAI.responseFile HTTP.ok200 [(HTTP.hContentType, "text/html; charset=utf-8")] fp Nothing

resourceFilePath :: Resource -> Maybe FilePath
resourceFilePath r = unsnoc r >>= \(dirTexts, fileText) ->
  traverse (Path.parseRelDir . toString) dirTexts >>= \dirs ->
    (Path.parseRelFile . toString) fileText >>= \file ->
      Path.addFileExtension ".html" file >>= \file' ->
        Just $ Path.toFilePath $ foldr (Path.</>) file' dirs

---  building a resource  ---

type Resource = [Text]

buildResource :: LogHandle -> Resource -> IO ()
buildResource _ _ = pure ()

---  build management  ---

data ResourceStatus = Building | Built deriving (Eq, Ord)

type ResourcesState = TVar (Map Resource ResourceStatus)

ensureResourceBuilt :: LogHandle -> ResourcesState -> Resource -> IO ()
ensureResourceBuilt l rs r = bracketOnError lock (const clear) go
  where
    lock :: IO Bool = atomically $
      readTVar rs >>= \m -> case (Map.lookup r m) of
        Nothing -> lockResourceBuilding rs r *> pure True
        Just Built -> pure False
        Just Building -> STM.retry
    clear :: IO () = atomically (clearResourceStatus rs r)
    go :: Bool -> IO () = \case
      False -> pure ()
      True -> buildResource l r *> atomically (recordResourceBuilt rs r)

clearResourceStatus :: ResourcesState -> Resource -> STM ()
clearResourceStatus rs r = STM.modifyTVar rs $ Map.delete r

recordResourceBuilt :: ResourcesState -> Resource -> STM ()
recordResourceBuilt rs r = STM.modifyTVar rs $ Map.insert r Built

lockResourceBuilding :: ResourcesState -> Resource -> STM ()
lockResourceBuilding rs r = STM.modifyTVar rs $ Map.insert r Building

---  file watch setup  ---

withWatches :: FSN.WatchManager -> FSN.Action -> [FilePath] -> IO a -> IO a
withWatches man act = fix \r ->
  \case
    [] -> id
    fp : fps -> withWatch man act fp . r fps

withWatch :: FSN.WatchManager -> FSN.Action -> FilePath -> IO a -> IO a
withWatch man act fp go =
  FSN.watchTree man fp (const True) act >>= \stop ->
    go `finally` stop

---  logging  ---

type LogHandle = STM.TChan String

writeToLog :: LogHandle -> String -> IO ()
writeToLog l s = atomically (STM.writeTChan l s)

withLogPrinting :: LogHandle -> IO a -> IO a
withLogPrinting l go = withAsync (printLogs l) \_ -> go

printLogs :: LogHandle -> IO a
printLogs l = forever printOne
  where
    printOne = pop >>= putStrLn
    pop = atomically $ STM.readTChan l

---  miscellania  ---

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = uncons (reverse xs) >>= \(x, xs') -> Just (reverse xs', x)
