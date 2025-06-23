module System.Console.Ask.Behaviour
    ( NewlineTiming (..)
    , Behaviour (..)
    , defaultBehaviour
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

data NewlineTiming = AfterPrompt | BeforePrompt | None deriving (Show, Eq)

data Behaviour = Behaviour
    { newlineTiming             :: NewlineTiming
    , defaultValueViewer        :: Text -> Text
    , mandatoryQuestionErrorMsg :: Maybe Text
    , invalidInputErrorMsg      :: Maybe Text
    }

defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { newlineTiming             = AfterPrompt
        , defaultValueViewer        = Text.append "Default: "
        , mandatoryQuestionErrorMsg = Just "This question is mandatory."
        , invalidInputErrorMsg      = Just "Invalid input."
        }
