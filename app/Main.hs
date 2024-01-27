module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import ListT
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import System.FilePath
import System.Directory
import qualified TD.GeneralResult as GR
import qualified TD.Data.Update as U
import qualified TD.Data.User as User
import qualified TD.Data.Chat as Chat
import qualified TD.Data.InputMessageContent as IMC
import qualified TD.Data.FormattedText as FT
import qualified TD.Query.SetLogVerbosityLevel as SLVL
import qualified TD.Query.GetMe as GM
import qualified TD.Query.CreatePrivateChat as CPC
import qualified TD.Query.SendMessage as SM

import Action
import Init

fromFoldableM :: Monad m => m [a] -> ListT m a
fromFoldableM = unfoldM $ \m -> m >>= f
    where f [] = pure Nothing
          f (x:xs) = pure $ Just (x, pure xs)

searchDirs :: [FilePath]
searchDirs = ["/home/amhsm6/tmp"]

data File = File { location :: FilePath
                 , savedTime :: UTCTime
                 }
                 deriving (Eq, Ord, Show)

scan :: IO (S.Set File)
scan = S.fromList <$> list
    where list = toList $ do
              dir <- fromFoldable searchDirs
              file <- fromFoldableM $ listDirectory dir
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

main :: IO ()
main = do
    initial <- scan
    watch initial
    {-start $ do
        send $ SLVL.SetLogVerbosityLevel $ Just 2
        auth

        send GM.GetMe
        let getMe = do
                x <- recv
                case x of
                    Just (GR.User user, _) -> pure user
                    _ -> getMe
        me <- getMe >>= unwrap . User._id

        send $ CPC.defaultCreatePrivateChat { CPC.user_id = Just me }
        let getChat = do
                x <- recv
                case x of
                    Just (GR.Chat chat, _) -> pure chat
                    _ -> getChat
        id <- getChat >>= unwrap . Chat._id

        liftIO $ print id

        send $ SM.defaultSendMessage { SM.chat_id = Just id
                                     , SM.input_message_content = Just $ IMC.InputMessageText (Just $ FT.defaultFormattedText { FT.text = Just $ T.pack "111" }) Nothing Nothing
                                     }-}
