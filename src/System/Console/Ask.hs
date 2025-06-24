module System.Console.Ask
    ( NewlineTiming (..)
    , DefaultValueStyle (..)
    , Behaviour (..)
    , AskT (..)
    , Askable (..)
    , Question
    , Prompt
    , Ask
    , defaultBehaviour
    , runAskT
    , runAsk'
    , runAsk
    , ask
    , askOrElse
    , askOptional
    , fromParsec
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Maybe                   (fromJust)
import           System.Console.Ask.Askable
import           System.Console.Ask.Behaviour
import           System.Console.Ask.Internal

newtype AskT m a = AskT { runAskT' :: Behaviour -> m a }

type Ask = AskT IO

runAskT :: Behaviour -> AskT m a -> m a
runAskT = flip runAskT'

runAsk' :: Ask a -> Behaviour -> IO a
runAsk' = runAskT'

runAsk :: Behaviour -> Ask a -> IO a
runAsk = runAskT

getBehaviour :: Monad m => AskT m Behaviour
getBehaviour = AskT { runAskT' = return }

instance Functor m => Functor (AskT m) where
    fmap f (AskT run) = AskT { runAskT' = fmap f . run }

instance Monad m => Applicative (AskT m) where
    pure a = AskT { runAskT' = const (pure a) }

    (AskT runF) <*> (AskT runA) = AskT $ \behaviour -> do
        f <- runF behaviour
        a <- runA behaviour
        return (f a)

instance Monad m => Monad (AskT m) where
    (AskT runA) >>= f = AskT $ \behaviour -> do
        a <- runA behaviour
        runAskT behaviour (f a)

instance MonadIO m => MonadIO (AskT m) where
    liftIO ma = AskT { runAskT' = const (liftIO ma) }

ask :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m a
ask question prompt =
    fmap fromJust $ getBehaviour >>=
        liftIO . ask_ True question prompt Nothing

askOrElse :: (MonadIO m, Askable a) => Question -> Prompt -> a -> AskT m a
askOrElse question prompt defaultVal =
    fmap fromJust $ getBehaviour >>=
        liftIO . ask_ True question prompt (Just defaultVal)

askOptional :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m (Maybe a)
askOptional question prompt =
    getBehaviour >>=
        liftIO . ask_ False question prompt Nothing
