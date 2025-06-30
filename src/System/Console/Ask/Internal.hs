{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.Ask.Internal
    ( Question
    , Prompt
    , readLineWithPrompt
    , ask_
    ) where

import           Control.Exception            (IOException, try)
import           Control.Lens                 ((^.))
import           Control.Monad                (when)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TextIO
import           System.Console.Ask.Askable   (Askable (..))
import           System.Console.Ask.Behaviour
import           System.IO                    (hFlush, stdout)

readLineWithPrompt :: Text -> IO (Maybe Text)
readLineWithPrompt prompt = do
    TextIO.putStr prompt
    hFlush stdout

    result <-
        try $ TextIO.getLine >>= \case
            "" -> pure Nothing
            x  -> pure (Just x)

    case result of
        Right result'           -> pure result'
        Left (_ :: IOException) -> pure Nothing


type Question = Text
type Prompt = Text

ask_ :: Askable a => Bool -> Question -> Prompt -> Maybe a -> Behaviour -> IO (Maybe a)
ask_ isMandatory question prompt defaultVal behaviour = do
    when (behaviour ^. newlineTiming == BeforePrompt)
        putNewLine

    case defaultVal of
        Nothing ->
            TextIO.putStrLn question

        Just defaultVal' ->
            let defaultValMessage = (behaviour ^. defaultValueViewer) (Text.show defaultVal') in
                case behaviour ^. defaultValueStyle of
                    OnQuestionLine ->
                        TextIO.putStrLn (question <> " (" <> defaultValMessage <> ")")

                    OnNewline ->
                        TextIO.putStrLn question >>
                            TextIO.putStrLn defaultValMessage

    result <-
        readLineWithPrompt prompt >>= \case
            Nothing ->
                case defaultVal of
                    Just defaultVal'          -> pure (Just (Just defaultVal'))
                    Nothing | not isMandatory -> pure (Just Nothing)
                    Nothing -> do
                        whenJust (behaviour ^. mandatoryQuestionErrorMsg)
                            TextIO.putStrLn

                        pure Nothing
            Just x ->
                case fromText x of
                    Just x' -> pure (Just (Just x'))
                    Nothing -> do
                        whenJust (behaviour ^. invalidInputErrorMsg)
                            TextIO.putStrLn

                        pure Nothing

    when (behaviour ^. newlineTiming == AfterPrompt)
        putNewLine

    case result of
        Just result' -> pure result'
        Nothing      -> ask_ isMandatory question prompt defaultVal behaviour

    where
        whenJust Nothing _  = pure ()
        whenJust (Just x) f = f x

        putNewLine = TextIO.putStrLn ""
