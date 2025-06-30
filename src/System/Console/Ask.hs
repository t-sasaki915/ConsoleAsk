module System.Console.Ask
    ( AskT (..)
    , Ask
    , runAskT
    , runAsk
    , ask
    , askOrElse
    , askOptional
    , ask'
    , askOrElse'
    , askOptional'
    , defaultBehaviour
    , withBehaviour
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Maybe                   (fromJust)
import           System.Console.Ask.Askable   (Askable)
import           System.Console.Ask.Behaviour (Behaviour, defaultBehaviour)
import           System.Console.Ask.Internal  (Prompt, Question, ask_)

newtype AskT m a = AskT (Behaviour -> m a)

runAskT :: Behaviour -> AskT m a -> m a
runAskT behaviour (AskT run) = run behaviour

type Ask = AskT IO

runAsk :: Behaviour -> Ask a -> IO a
runAsk = runAskT

instance Functor m => Functor (AskT m) where
    fmap f (AskT run) = AskT (fmap f . run)

instance Monad m => Applicative (AskT m) where
    pure = AskT . const . pure

    (AskT runF) <*> (AskT runA) = AskT $ \behaviour -> do
        f <- runF behaviour
        a <- runA behaviour
        pure (f a)

instance Monad m => Monad (AskT m) where
    (AskT runA) >>= f = AskT $ \behaviour -> do
        a <- runA behaviour
        runAskT behaviour (f a)

instance MonadIO m => MonadIO (AskT m) where
    liftIO = AskT . const . liftIO

ask' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m a
ask' question prompt =
    fromJust <$> AskT (liftIO . ask_ True question prompt Nothing)

askOrElse' :: (MonadIO m, Askable a) => Question -> a -> Prompt -> AskT m a
askOrElse' question defaultVal prompt =
    fromJust <$> AskT (liftIO . ask_ True question prompt (Just defaultVal))

askOptional' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m (Maybe a)
askOptional' question prompt =
    AskT (liftIO . ask_ False question prompt Nothing)

ask :: (MonadIO m, Askable a) => Question -> AskT m a
ask question = ask' question "> "

askOrElse :: (MonadIO m, Askable a) => Question -> a -> AskT m a
askOrElse question defaultVal = askOrElse' question defaultVal "> "

askOptional :: (MonadIO m, Askable a) => Question -> AskT m (Maybe a)
askOptional question = askOptional' question "> "

withBehaviour :: Behaviour -> AskT m a -> AskT m a
withBehaviour behaviour = AskT . const  . runAskT behaviour
