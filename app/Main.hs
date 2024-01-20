module Main where

import Control.Monad
import Control.Monad.List
import Data.Time
import System.FilePath
import System.Directory
import qualified TD.GeneralResult as GR
import qualified TD.Data.Update as U
import TD.Query.SetLogVerbosityLevel

import Action
import Init

searchDirs :: [FilePath]
searchDirs = ["/opt/Heroes3/games"]

data File = File { location :: FilePath
                 , savedTime :: UTCTime
                 }
                 deriving Show

scan :: IO [File]
scan = runListT $ do
    dir <- fromList searchDirs
    file <- ListT $ listDirectory dir
    let path = dir </> file

    time <- liftIO $ getModificationTime path
    pure $ File path time

{-watch :: Action ()
watch = do
    curr <- scan
    prev <- atomically $ readTVar state
    let changed = filter (\(x, y) -> savedTime x > savedTime y) $ zip curr prev
    forM_ changed $ \(new, _) -> do
        print $ location new-}

loop :: Action ()
loop = do
    x <- recv >>= unwrap >>= pure . fst
    case x of
        GR.Update (U.UpdateAuthorizationState (Just state)) -> auth state
        _ -> pure ()

main :: IO ()
main = fresh $ do
    send $ SetLogVerbosityLevel $ Just 2
    inf loop
