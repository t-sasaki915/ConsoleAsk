module System.Console.Ask
    ( NewlineTiming (..)
    , Behaviour (..)
    , Ask (..)
    , Askable (..)
    , Question
    , Prompt
    , defaultBehaviour
    , runAsk
    , getBehaviour
    , ask
    , askOrElse
    , askOptional
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           System.Console.Ask.Askable
import           System.Console.Ask.Behaviour
import           System.Console.Ask.Internal

newtype Ask a = Ask { runAsk' :: Behaviour -> IO a }

runAsk :: Behaviour -> Ask a -> IO a
runAsk = flip runAsk'

getBehaviour :: Ask Behaviour
getBehaviour = Ask { runAsk' = return }

instance Functor Ask where
    fmap f (Ask run) = Ask { runAsk' = fmap f . run }

instance Applicative Ask where
    pure a = Ask { runAsk' = const (pure a) }

    (Ask runF) <*> (Ask runA) = Ask $ \behaviour -> do
        f <- runF behaviour
        a <- runA behaviour
        return (f a)

instance Monad Ask where
    (Ask runA) >>= f = Ask $ \behaviour -> do
        a <- runA behaviour
        runAsk behaviour (f a)

instance MonadIO Ask where
    liftIO ma = Ask { runAsk' = const ma }

ask :: Askable a => Question -> Prompt -> Ask a
ask question prompt =
    getBehaviour >>=
        liftIO . ask_ fromText question prompt Nothing

askOrElse :: Askable a => Question -> Prompt -> a -> Ask a
askOrElse question prompt defaultVal =
    getBehaviour >>=
        liftIO . ask_ fromText question prompt (Just defaultVal)

askOptional :: Askable a => Question -> Prompt -> Ask (Maybe a)
askOptional question prompt =
    getBehaviour >>=
        liftIO . askMaybe_ fromText question prompt
