module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.List
import Control.Concurrent
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import System.FilePath
import System.Directory
import qualified TD.GeneralResult as GR
import qualified TD.Data.Update as U
import qualified TD.Data.Chats as CS
import TD.Query.SetLogVerbosityLevel
import TD.Query.SearchChatsOnServer

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

watch :: S.Set File -> Action ()
watch prev = do
    curr <- liftIO scan

    let changed = S.elems $ S.difference curr prev
    forM_ changed $ \file -> do
        liftIO $ print file

    watch curr

main :: IO ()
main = do
    initial <- scan
    fresh $ do
        send $ SetLogVerbosityLevel $ Just 2
        auth

        send $ SearchChatsOnServer (Just $ T.pack "Saved Messages") $ Just 1
        let getChat = do
                liftIO $ putStrLn "Trying.."
                x <- recv
                liftIO $ print $ fst <$> x
                case x of
                    Just (GR.Chats (CS.Chats (Just 1) (Just [id])), _) -> pure id
                    Just (GR.Chats _, _) -> end
                    _ -> getChat
        id <- getChat

        liftIO $ print id
