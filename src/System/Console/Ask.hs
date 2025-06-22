module System.Console.Ask
    ( NewlineTiming (..)
    , Behaviour (..)
    , Ask (..)
    , defaultBehaviour
    , runAsk
    ) where

import           Control.Monad                (join)
import           Control.Monad.IO.Class       (MonadIO (..))
import           System.Console.Ask.Behaviour

data Ask a = Ask { runAsk' :: Behaviour -> IO a }

runAsk :: Behaviour -> Ask a -> IO a
runAsk = flip runAsk'

instance Functor Ask where
    fmap f ma = ma >>= return . f

instance Applicative Ask where
    pure a = Ask { runAsk' = const (pure a) }

    liftA2 f ma mb = do
        a <- ma
        b <- mb
        return (f a b)

instance Monad Ask where
    (>>=) fa f = Ask { runAsk' = \behaviour -> runAsk behaviour (join (liftIO (f <$> runAsk behaviour fa))) }

instance MonadIO Ask where
    liftIO ma = Ask { runAsk' = const ma }
