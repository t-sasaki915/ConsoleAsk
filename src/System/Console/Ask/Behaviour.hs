{-# LANGUAGE TemplateHaskell #-}

module System.Console.Ask.Behaviour
    ( NewlineTiming (..)
    , DefaultValueStyle (..)
    , Behaviour
    , defaultBehaviour
    , newlineTiming
    , defaultValueStyle
    , defaultValueViewer
    , mandatoryQuestionErrorMsg
    , invalidInputErrorMsg
    , set
    ) where

import           Control.Lens (makeLenses, set)
import           Data.Text    (Text)

data NewlineTiming = AfterPrompt | BeforePrompt | None deriving Eq

data DefaultValueStyle = OnQuestionLine | OnNewline deriving Eq

data Behaviour = Behaviour
    { _newlineTiming             :: NewlineTiming
    , _defaultValueStyle         :: DefaultValueStyle
    , _defaultValueViewer        :: Text -> Text
    , _mandatoryQuestionErrorMsg :: Maybe Text
    , _invalidInputErrorMsg      :: Maybe Text
    }

makeLenses ''Behaviour

defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { _newlineTiming             = AfterPrompt
        , _defaultValueStyle         = OnQuestionLine
        , _defaultValueViewer        = ("Default: " <>)
        , _mandatoryQuestionErrorMsg = Just "This question is mandatory."
        , _invalidInputErrorMsg      = Just "Invalid input."
        }
