module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import ListT
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import System.Exit
import System.FilePath
import System.Directory
import qualified TD.GeneralResult as GR
import qualified TD.Data.User as User
import qualified TD.Data.Chat as Chat
import qualified TD.Data.InputFile as IF
import qualified TD.Data.FormattedText as FT
import qualified TD.Data.InputMessageContent as IMC
import qualified TD.Data.Update as U
import qualified TD.Data.Message as M
import qualified TD.Data.MessageContent as MC
import qualified TD.Data.Document as D
import qualified TD.Data.File as F
import qualified TD.Data.RemoteFile as RF
import qualified TD.Query.SetLogVerbosityLevel as SLVL
import qualified TD.Query.GetMe as GM
import qualified TD.Query.CreatePrivateChat as CPC
import qualified TD.Query.SendMessage as SM
import qualified TD.Query.DownloadFile as DF

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

              liftIO $ do
                  dir <- doesDirectoryExist path
                  when dir $ putStrLn "error: directories are not supported" >> exitFailure

              time <- liftIO $ getModificationTime path
              pure $ File path time

getSavedMessages :: Action Int
getSavedMessages = do
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

    pure id

watchLocal :: Int -> S.Set File -> Action ()
watchLocal id prev = do
    curr <- liftIO scan

    let changed = S.elems $ S.difference curr prev
    forM_ changed $ \file -> do
        liftIO $ putStrLn $ "Updated: " ++ location file
        let document = IF.InputFileLocal $ Just $ T.pack $ location file
            caption = FT.defaultFormattedText { FT.text = Just $ T.pack $ "hgwatch " ++ location file }
            content = IMC.InputMessageDocument (Just document) Nothing (Just True) (Just caption)
        send $ SM.defaultSendMessage { SM.chat_id = Just id
                                     , SM.input_message_content = Just content
                                     }

    watchLocal id curr

watchRemote :: Int -> Action ()
watchRemote id = inf $ do
    liftIO $ threadDelay 1000
    x <- recv
    case x of
        Just (GR.Update (U.UpdateNewMessage (Just msg)), _) -> do
            unless (M.chat_id msg == Just id) mzero

            (id, path) <- case M.content msg of
                               Just (MC.MessageDocument (Just (D.Document _ _ _ _ (Just (F.File (Just id) _ _ _ _)))) (Just (FT.FormattedText (Just caption) _))) ->
                                   case words $ T.unpack caption of
                                       ["hgwatch", path] -> pure (id, path)
                                       _ -> mzero
                               _ -> mzero

            send $ DF.defaultDownloadFile { DF.file_id = Just id, DF.priority = Just 32, DF.synchronous = Just True }
            liftIO $ threadDelay 100000
            let getFile = do
                    x <- recv
                    liftIO $ print $ fst <$> x
                    case x of
                        Just (GR.File _ _ _ (Just )), _) -> 
                        _ -> getFile
            file <- getFile

            liftIO $ putStrLn "done"
        _ -> pure ()

main :: IO ()
main = do
    initial <- scan
    start $ do
        send $ SLVL.SetLogVerbosityLevel $ Just 2
        auth
        id <- getSavedMessages

        fork $ watchRemote id
        watchLocal id initial
