{-# LANGUAGE FlexibleInstances #-}

module Action
    ( Action, runAction
    , unwrap, end, inf, Checkable, check
    , send, recv
    , liftIO
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (ToJSON)
import qualified TD.Lib as TDL
import qualified TD.GeneralResult as TDL

type Action = ReaderT TDL.Client (MaybeT IO)

runAction :: TDL.Client -> Action a -> IO (Maybe a)
runAction client act = runMaybeT $ runReaderT act client

unwrap :: Maybe a -> Action a
unwrap = ReaderT . const . MaybeT . pure

end :: Action a
end = unwrap Nothing

inf :: Action a -> Action b
inf act = do
    x <- ask
    liftIO $ runAction x act
    inf act

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
