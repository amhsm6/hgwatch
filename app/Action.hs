{-# LANGUAGE FlexibleInstances #-}

module Action
    ( Action, runAction
    , unwrap, inf, Checkable, check
    , send, recv
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson (ToJSON)
import qualified TD.Lib as TDL
import qualified TD.GeneralResult as TDL

type Action = ReaderT TDL.Client (ExceptT () IO)

runAction :: TDL.Client -> Action a -> IO (Maybe a)
runAction client m = either (const Nothing) Just <$> runExceptT (runReaderT m client)

unwrap :: Maybe a -> Action a
unwrap Nothing = mzero
unwrap (Just x) = pure x

inf :: Action a -> Action b
inf m = do
    x <- ask
    liftIO $ runAction x m
    inf m

class Checkable a where
    check :: a -> Action ()

instance Checkable Bool where
    check = guard

instance Checkable (Maybe Bool) where
    check = unwrap >=> guard

send :: ToJSON a => a -> Action ()
send x = ask >>= \client -> liftIO $ TDL.send client x

recv :: Action (Maybe (TDL.GeneralResult, Maybe TDL.Extra))
recv = ask >>= \client -> liftIO $ TDL.receive client
