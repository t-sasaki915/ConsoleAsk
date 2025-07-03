{-|
Module      : System.Console.Ask.Behaviour
Copyright   : 2025 Toma Sasaki
Licence     : MIT
Maintainer  : netst915@gmail.com
Portability : Portable

@System.Console.Ask.Behaviour@ provides 'Behaviour', its fields and 'set'.
You should import this module if you want to customise the behaviour of ConsoleAsk.
-}

{-# LANGUAGE TemplateHaskell #-}

module System.Console.Ask.Behaviour
    ( NewlineTiming (..)
    , DefaultValueStyle (..)
    , Behaviour (..)
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

{-|
'NewlineTiming' is the timing of ConsoleAsk outputs newline between questions.

'AfterPrompt'

@
Question1?
> ABC

Question2?
> DEF

@

'BeforePrompt'

@


Question1?
> ABC

Question2?
> DEF
@

'None'

@
Question1?
> ABC
Question2?
> DEF
@
-}
data NewlineTiming = AfterPrompt | BeforePrompt | None deriving Eq

{-|
'DefaultValueStyle' is the setting of where the default value is displayed.

'OnQuestionLine'

@
Question1? (Default: \"ABC\")
>
@

'OnNewline'

@
Question1?
Default: \"ABC\"
>
@
-}
data DefaultValueStyle = OnQuestionLine | OnNewline deriving Eq

{-|
'Behaviour' specifies ConsoleAsk behaviours.
'set' is useful for editing already existing 'Behaviour' definitions field-by-field.

@
let customBehaviour = 'set' 'System.Console.Ask.Behaviour.newlineTiming' 'BeforePrompt' 'System.Console.Ask.Behaviour.defaultBehaviour'
@
-}
data Behaviour = Behaviour
    { -- | Please see 'NewlineTiming'.
      _newlineTiming             :: NewlineTiming
      -- | Please see 'DefaultValueStyle'.
    , _defaultValueStyle         :: DefaultValueStyle
      {-|
      '_defaultValueViewer' is the message displayed if the question has a default value.
      It is a function whose argument is the default value of the question.

      Assume @'_defaultValueViewer' = ("Default Value is: " <>)@,

      >>> askOrElse "Do you need notifications?" False :: Ask Bool
      Do you need notifications? (Default Value is: False)
      >
      -}
    , _defaultValueViewer        :: Text -> Text
      {-|
      '_mandatoryQuestionErrorMsg' is the message displayed if the user has not answered the question even though it is mandatory.
      If @Nothing@, ConsoleAsk will not display the message.

      Assume @'_mandatoryQuestionErrorMsg' = Just "PLEASE ANSWER THIS QUESTION!!!"@,

      >>> ask "What is your name?" :: Ask Text
      What is your name?
      >
      PLEASE ANSWER THIS QUESTION!!!
      -}
    , _mandatoryQuestionErrorMsg :: Maybe Text
      {-|
      '_invalidInputErrorMsg' is the message displayed if the user enters an invalid value.
      If @Nothing@, ConsoleAsk will not display the message.

      Assume @'_invalidInputErrorMsg' = Just "????????"@,

      >>> ask "How old are you?" :: Ask Int
      How old are you?
      > abc
      ????????
      -}
    , _invalidInputErrorMsg      :: Maybe Text
    }

makeLenses ''Behaviour

{-|
'defaultBehaviour' is the default definition of ConsoleAsk behaviour.

@
'defaultBehaviour' = 'Behaviour'
    { '_newlineTiming'             = 'AfterPrompt'
    , '_defaultValueStyle'         = 'OnQuestionLine'
    , '_defaultValueViewer'        = (\"Default\" <>)
    , '_mandatoryQuestionErrorMsg' = Just \"This question is mandatory.\"
    , '_invalidInputErrorMsg'      = Just \"Invalid input.\"
    }
@
-}
defaultBehaviour :: Behaviour
defaultBehaviour =
    Behaviour
        { _newlineTiming             = AfterPrompt
        , _defaultValueStyle         = OnQuestionLine
        , _defaultValueViewer        = ("Default: " <>)
        , _mandatoryQuestionErrorMsg = Just "This question is mandatory."
        , _invalidInputErrorMsg      = Just "Invalid input."
        }
