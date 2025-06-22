module System.Console.Ask
    ( NewlineTiming (..)
    , Behaviour (..)
    , defaultBehaviour
    ) where

import           Control.Monad (join)

data NewlineTiming = AfterPrompt | BeforePrompt deriving (Show, Eq)

newtype Behaviour = Behaviour
    { newlineTiming :: NewlineTiming
    } deriving (Show, Eq)

defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { newlineTiming = AfterPrompt
        }

newtype AskT m a = AskT { runAskT :: Behaviour -> m a }

instance Monad a => Functor (AskT a) where
    fmap f fa = fa >>= (pure . f)

instance Monad a => Applicative (AskT a) where
    pure a = AskT { runAskT = const (pure a) }

    liftA2 f fa fb = do
        a <- fa
        b <- fb
        return (f a b)

instance Monad a => Monad (AskT a) where
  (>>=) (AskT a) f = AskT { runAskT = \behaviour -> let aaaa = (a behaviour >>= \aa -> return $ f aa) in join aaaa }
