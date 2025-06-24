module System.Console.Ask.Behaviour
    ( NewlineTiming (..)
    , DefaultValueStyle (..)
    , Behaviour (..)
    , defaultBehaviour
    ) where

import           Data.Text (Text)

data NewlineTiming = AfterPrompt | BeforePrompt | None deriving Eq

data DefaultValueStyle = OnQuestionLine | OnNewline deriving Eq

data Behaviour = Behaviour
    { newlineTiming             :: NewlineTiming
    , defaultValueStyle         :: DefaultValueStyle
    , defaultValueViewer        :: Text -> Text
    , mandatoryQuestionErrorMsg :: Maybe Text
    , invalidInputErrorMsg      :: Maybe Text
    }

defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { newlineTiming             = AfterPrompt
        , defaultValueStyle         = OnQuestionLine
        , defaultValueViewer        = ("Default: " <>)
        , mandatoryQuestionErrorMsg = Just "This question is mandatory."
        , invalidInputErrorMsg      = Just "Invalid input."
        }
