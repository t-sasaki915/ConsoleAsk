{-|
Module      : System.Console.Ask
Copyright   : 2025 Toma Sasaki
Licence     : MIT
Maintainer  : netst915@gmail.com
Portability : Portable

@System.Console.Ask@ is the main module of ConsoleAsk.
-}

module System.Console.Ask
    ( AskT (..)
    , Ask
    , runAskT
    , runAsk
    , ask
    , askOrElse
    , askOptional
    , ask'
    , askOrElse'
    , askOptional'
    , defaultBehaviour
    , withBehaviour
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Maybe                   (fromJust)
import           System.Console.Ask.Askable   (Askable)
import           System.Console.Ask.Behaviour (Behaviour, defaultBehaviour)
import           System.Console.Ask.Internal  (Prompt, Question, ask_)

-- | 'AskT' is a kind of @MonadIO@ with 'Behaviour' inside.
newtype AskT m a = AskT (Behaviour -> m a)

{-|
'runAskT' unwraps an 'AskT' monad.

@
askQuestion :: MonadIO m => 'AskT' m Text
askQuestion = 'ask' \"Question1\"

askQuestionMonadIO :: MonadIO m => m Text
askQuestionMonadIO = 'runAskT' 'defaultBehaviour' askQuestion
@
-}
runAskT :: Behaviour -> AskT m a -> m a
runAskT behaviour (AskT run) = run behaviour

-- | 'Ask' is the same as 'AskT', but @m@ is fixed to @IO@.
type Ask = AskT IO

-- | 'runAsk' is the same as 'runAskT', but @m@ is fixed to @IO@.
runAsk :: Behaviour -> Ask a -> IO a
runAsk = runAskT

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

{-|
'ask'' is the same as 'ask', but you can specify the prompt.

>>> ask' "What is your name?" "Text> " :: Ask Text
What is your name?
Text>
-}
ask' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m a
ask' question prompt =
    fromJust <$> AskT (liftIO . ask_ True question prompt Nothing)

{-|
'askOrElse'' is the same as 'askOrElse', but you can specify the prompt.

>>> askOrElse' "Do you need notifications?" False "Y/N> " :: Ask Bool
Do you need notifications? (Default: False)
Y/N>
-}
askOrElse' :: (MonadIO m, Askable a) => Question -> a -> Prompt -> AskT m a
askOrElse' question defaultVal prompt =
    fromJust <$> AskT (liftIO . ask_ True question prompt (Just defaultVal))

{-|
'askOptional'' is the same as 'askOptional', but you can specify the prompt.

>>> askOptional' "How old are you?" "Int> " :: Ask (Maybe Int)
How old are you?
Int>
-}
askOptional' :: (MonadIO m, Askable a) => Question -> Prompt -> AskT m (Maybe a)
askOptional' question prompt =
    AskT (liftIO . ask_ False question prompt Nothing)

{-|
'ask' asks user a mandatory question.

>>> ask "What is your name?" :: Ask Text

@
What is your name?
>
This question is mandatory.

What is your name?
>
@
-}
ask :: (MonadIO m, Askable a) => Question -> AskT m a
ask question = ask' question "> "

{-|
'askOrElse' asks user a mandatory question with a default value.

>>> askOrElse "Do you need notifications?" False :: Ask Bool
Do you need notifications? (Default: False)
>
-}
askOrElse :: (MonadIO m, Askable a) => Question -> a -> AskT m a
askOrElse question defaultVal = askOrElse' question defaultVal "> "

{-|
'askOptional' asks user an optional question.

>>> askOptional "How old are you?" :: Ask (Maybe Int)
How old are you?
>
-}
askOptional :: (MonadIO m, Askable a) => Question -> AskT m (Maybe a)
askOptional question = askOptional' question "> "

{-|
'withBehaviour' attaches a behaviour to 'AskT'.
The behaviour specified to 'runAskT' will be ignored.

@
data Directories = Directories
    { windowsRootDir :: FilePath
    , installDir     :: FilePath
    } deriving Show

askDirectories :: 'Ask' Directories
askDirectories = do
    let customBehaviour = 'System.Console.Ask.Behaviour.set' 'System.Console.Ask.Behaviour.defaultValueStyle' 'System.Console.Ask.Behaviour.OnNewline' 'defaultBehaviour'

    Directories
        \<$\> 'askOrElse' \"Windows root directory?\" \"C:\\\\Windows\\\\"
        \<*\> 'withBehaviour' customBehaviour ('askOrElse' \"Install directory?\" \"C:\\\\Program Files\\\\net\\\\t_sasaki\\\\useful_tool\\\\\")

main :: IO ()
main = do
    dirs <- 'runAsk' 'defaultBehaviour' askDirectories
    print dirs
@

@
Windows root directory? (Default: \"C:\\Windows\\")
>

Install directory?
Default: \"C:\\Program Files\\net\\t_sasaki\\useful_tool\\\"
>
@
-}
withBehaviour :: Behaviour -> AskT m a -> AskT m a
withBehaviour behaviour = AskT . const  . runAskT behaviour
