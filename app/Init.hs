module Init
    ( start, auth
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import System.Environment
import TD.Lib (create)
import TD.GeneralResult
import TD.Data.Update
import TD.Data.AuthorizationState
import TD.Query.SetTdlibParameters
import TD.Query.SetAuthenticationPhoneNumber
import TD.Query.CheckAuthenticationCode

import Action

start :: Action a -> IO ()
start m = create >>= \x -> void $ runAction x m

auth :: Action ()
auth = do
    x <- recv
    case x of
        Just (Update (UpdateAuthorizationState (Just AuthorizationStateReady)), _) -> pure ()
        Just (Update (UpdateAuthorizationState (Just state)), _) -> exec state >> auth
        _ -> auth

exec :: AuthorizationState -> Action ()
exec AuthorizationStateWaitTdlibParameters = do
    apiId <- liftIO $ getEnv "API_ID"
    apiHash <- liftIO $ getEnv "API_HASH"
    send $ defaultSetTdlibParameters { database_directory = Just $ T.pack "db"
                                     , api_id = Just $ read apiId
                                     , api_hash = Just $ T.pack apiHash
                                     , device_model = Just $ T.pack "Haskell"
                                     , system_language_code = Just $ T.pack "en"
                                     , application_version = Just $ T.pack "1.0.0"
                                     }
exec AuthorizationStateWaitPhoneNumber = do
    phone <- liftIO $ getEnv "PHONE_NUMBER"
    send $ defaultSetAuthenticationPhoneNumber { phone_number = Just $ T.pack phone }
exec x@(AuthorizationStateWaitCode _) = do
    liftIO $ putStrLn "type authentication code"
    code <- liftIO $ getLine
    when (null code) $ liftIO (putStrLn "Wrong code") >> exec x
    send $ CheckAuthenticationCode { code = Just $ T.pack code }
exec AuthorizationStateReady = pure ()
exec x = do
    liftIO $ putStrLn $ "error: unknown authorization state:\n" ++ show x
    mzero
