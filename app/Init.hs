module Init
    ( fresh, auth
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import System.Environment
import TD.Lib (create)
import TD.Data.AuthorizationState
import TD.Query.SetTdlibParameters
import TD.Query.SetAuthenticationPhoneNumber
import TD.Query.CheckAuthenticationCode

import Action

fresh :: Action a -> IO ()
fresh m = create >>= \x -> void $ runAction x m

auth :: AuthorizationState -> Action ()
auth AuthorizationStateWaitTdlibParameters = do
    apiId <- liftIO $ getEnv "API_ID"
    apiHash <- liftIO $ getEnv "API_HASH"
    send $ defaultSetTdlibParameters { database_directory = Just $ T.pack "db"
                                     , api_id = Just $ read apiId
                                     , api_hash = Just $ T.pack apiHash
                                     , device_model = Just $ T.pack "Haskell"
                                     , system_language_code = Just $ T.pack "en"
                                     , application_version = Just $ T.pack "1.0.0"
                                     }
auth AuthorizationStateWaitPhoneNumber = do
    phone <- liftIO $ getEnv "PHONE_NUMBER"
    send $ defaultSetAuthenticationPhoneNumber { phone_number = Just $ T.pack phone }
auth x@(AuthorizationStateWaitCode _) = do
    liftIO $ putStrLn "Type authentication code"
    code <- liftIO $ getLine
    when (null code) $ liftIO (putStrLn "Wrong code") >> auth x
    send $ CheckAuthenticationCode { code = Just $ T.pack code }
auth AuthorizationStateReady = pure ()
auth x = do
    liftIO $ putStrLn $ concat [ "Unknown Authorization State:\n"
                               , show x
                               , "\n --> Skipping"
                               ]
    end
