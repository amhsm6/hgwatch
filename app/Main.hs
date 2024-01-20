module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.List
import Control.Concurrent
import qualified Data.Set as S
import Data.Time
import System.FilePath
import System.Directory
import qualified TD.GeneralResult as GR
import qualified TD.Data.Update as U
import TD.Query.SetLogVerbosityLevel

import Action
import Init

searchDirs :: [FilePath]
searchDirs = ["/home/amhsm6/tmp"]

data File = File { location :: FilePath
                 , savedTime :: UTCTime
                 }
                 deriving (Eq, Ord, Show)

scan :: IO (S.Set File)
scan = S.fromList <$> list
    where list = runListT $ do
              dir <- fromList searchDirs
              file <- ListT $ listDirectory dir
              let path = dir </> file

              time <- liftIO $ getModificationTime path
              pure $ File path time

watch :: S.Set File -> IO ()
watch prev = do
    curr <- liftIO scan

    let changed = S.elems $ S.difference curr prev
    forM_ changed $ \file -> do
        liftIO $ print file

    watch curr

loop :: Action ()
loop = do
    x <- recv
    case x of
        Just (GR.Update (U.UpdateAuthorizationState (Just state)), _) -> auth state
        _ -> pure ()

main :: IO ()
main = do
    initial <- scan
    watch initial
    {-fresh $ do
        send $ SetLogVerbosityLevel $ Just 2

        x <- ask
        liftIO $ forkIO $ void $ runAction x $ watch initial

        inf loop-}
