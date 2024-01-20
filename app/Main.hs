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
    curr <- scan

    let changed = S.difference curr prev
    unless (S.null changed) $ putStrLn $ S.showTree changed

    watch curr

loop :: Action ()
loop = do
    x <- recv >>= unwrap >>= pure . fst
    case x of
        GR.Update (U.UpdateAuthorizationState (Just state)) -> auth state
        _ -> pure ()

main :: IO ()
main = do
    initial <- scan
    fresh $ do
        send $ SetLogVerbosityLevel $ Just 2

        x <- ask
        liftIO $ forkIO $ void $ runAction x $ liftIO $ watch initial

        inf loop
