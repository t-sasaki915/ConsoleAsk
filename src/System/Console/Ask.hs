module System.Console.Ask
    ( AskT (..)
    , Question
    , Prompt
    , Ask
    , defaultBehaviour
    , runAskT
    , runAsk
    , defaultPrompt
    , ask'
    , askOrElse'
    , askOptional'
    , ask
    , askOrElse
    , askOptional
    , withBehaviour
    , liftIO
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Maybe                   (fromJust)
import           System.Console.Ask.Askable
import           System.Console.Ask.Behaviour
import           System.Console.Ask.Internal

newtype AskT m a = AskT (Behaviour -> m a)

runAskT :: Behaviour -> AskT m a -> m a
runAskT behaviour (AskT run) = run behaviour

type Ask = AskT IO

runAsk :: Behaviour -> Ask a -> IO a
runAsk = runAskT

getBehaviour :: Monad m => AskT m Behaviour
getBehaviour = AskT pure

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

defaultPrompt :: Prompt
defaultPrompt = "> "

ask' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m a
ask' question prompt =
    fmap fromJust $
        getBehaviour >>=
            liftIO . ask_ True question prompt Nothing

askOrElse' :: (MonadIO m, Askable a) => Question -> a -> Prompt -> AskT m a
askOrElse' question defaultVal prompt =
    fmap fromJust $
        getBehaviour >>=
            liftIO . ask_ True question prompt (Just defaultVal)

askOptional' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m (Maybe a)
askOptional' question prompt =
    getBehaviour >>=
        liftIO . ask_ False question prompt Nothing

ask :: (MonadIO m, Askable a) => Question -> AskT m a
ask question = ask' question defaultPrompt

askOrElse :: (MonadIO m, Askable a) => Question -> a -> AskT m a
askOrElse question defaultVal = askOrElse' question defaultVal defaultPrompt

askOptional :: (MonadIO m, Askable a) => Question -> AskT m (Maybe a)
askOptional question = askOptional' question defaultPrompt

withBehaviour :: Behaviour -> AskT m a -> AskT m a
withBehaviour behaviour = AskT . const  . runAskT behaviour
