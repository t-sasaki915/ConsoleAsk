module System.Console.Ask
    ( NewlineTiming (..)
    , Behaviour (..)
    , Ask (..)
    , Question
    , Prompt
    , defaultBehaviour
    , runAsk
    , getBehaviour
    , ask
    , askMaybe
    , askText
    , askTextMaybe
    ) where

import           Control.Monad                (join)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Functor                 ((<&>))
import           Data.Text                    (Text)
import           System.Console.Ask.Behaviour
import           System.Console.Ask.Internal

newtype Ask a = Ask { runAsk' :: Behaviour -> IO a }

runAsk :: Behaviour -> Ask a -> IO a
runAsk = flip runAsk'

getBehaviour :: Ask Behaviour
getBehaviour = Ask { runAsk' = return }

instance Functor Ask where
    fmap f ma = Ask { runAsk' = \behaviour -> liftIO (runAsk behaviour ma <&> f) }

instance Applicative Ask where
    pure a = Ask { runAsk' = const (pure a) }

    liftA2 f ma mb = ma >>= \a -> f a <$> mb

instance Monad Ask where
    (>>=) fa f = Ask { runAsk' = \behaviour -> runAsk behaviour (join (liftIO (f <$> runAsk behaviour fa))) }

instance MonadIO Ask where
    liftIO ma = Ask { runAsk' = const ma }

ask :: Show a => (Text -> Maybe a) -> Question -> Prompt -> Maybe a -> Ask a
ask func question prompt defaultVal =
    getBehaviour >>=
        liftIO . ask_ func question prompt defaultVal

askMaybe :: Show a => (Text -> Maybe a) -> Question -> Prompt -> Ask (Maybe a)
askMaybe func question prompt =
    getBehaviour >>=
        liftIO . askMaybe_ func question prompt

askText :: Question -> Prompt -> Maybe Text -> Ask Text
askText = ask Just

askTextMaybe :: Question -> Prompt -> Ask (Maybe Text)
askTextMaybe = askMaybe Just
