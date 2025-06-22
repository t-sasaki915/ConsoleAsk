module System.Console.Ask.Internal
    ( Question
    , Prompt
    , readLineWithPrompt
    , askMaybe_
    , ask_
    , askMaybe__
    , whenJust
    ) where

import           Control.Monad                (when)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TextIO
import           System.Console.Ask.Behaviour (Behaviour (..),
                                               NewlineTiming (..))
import           System.IO                    (hFlush, stdout)

readLineWithPrompt :: Text -> IO (Maybe Text)
readLineWithPrompt prompt = do
    TextIO.putStr prompt
    hFlush stdout

    TextIO.getLine >>= \case
        "" -> return Nothing
        x  -> return (Just x)

type Question = Text
type Prompt = Text

askMaybe_ :: Show a => (Text -> Maybe a) -> Question -> Prompt -> Behaviour -> IO (Maybe a)
askMaybe_ func question prompt =
    askMaybe__ func question prompt Nothing

ask_ :: Show a => (Text -> Maybe a) -> Question -> Prompt -> Maybe a -> Behaviour -> IO a
ask_ func question prompt defaultVal behaviour =
    askMaybe__ func question prompt defaultVal behaviour >>= \case
        Just value -> return value
        Nothing    -> ask_ func question prompt defaultVal behaviour

askMaybe__ :: Show a => (Text -> Maybe a) -> Question -> Prompt -> Maybe a -> Behaviour -> IO (Maybe a)
askMaybe__ func question prompt defaultVal behaviour = do
    when (newlineTiming behaviour == BeforePrompt) $
        TextIO.putStrLn ""

    TextIO.putStrLn question
    whenJust defaultVal $ \defaultVal' ->
        TextIO.putStrLn ("Default: " `Text.append` Text.show defaultVal')
    result <-
        readLineWithPrompt prompt >>= \case
            Nothing ->
                case defaultVal of
                    Just defaultVal' -> return (Just defaultVal')
                    Nothing          -> return Nothing
            Just x ->
                case func x of
                    Just x' -> return (Just x')
                    Nothing -> askMaybe__ func question prompt defaultVal behaviour

    when (newlineTiming behaviour == AfterPrompt) $
        TextIO.putStrLn ""

    return result

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = return ()
whenJust (Just x) f = f x
