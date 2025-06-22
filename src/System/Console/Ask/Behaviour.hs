module System.Console.Ask.Behaviour
    ( NewlineTiming (..)
    , Behaviour (..)
    , defaultBehaviour
    ) where

data NewlineTiming = AfterPrompt | BeforePrompt deriving (Show, Eq)

newtype Behaviour = Behaviour
    { newlineTiming :: NewlineTiming
    } deriving (Show, Eq)

defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { newlineTiming = AfterPrompt
        }
