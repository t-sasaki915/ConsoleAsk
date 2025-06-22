module System.Console.Ask.Internal (readLineWithPrompt) where

import           Control.Monad                (when)
import           Data.Text                    (Text)
import qualified Data.Text.IO                 as TextIO
import           System.Console.Ask.Behaviour (Behaviour (..),
                                               NewlineTiming (..))
import           System.IO                    (hFlush, stdout)

readLineWithPrompt :: Behaviour -> Text -> IO (Maybe Text)
readLineWithPrompt behaviour prompt = do
    when (newlineTiming behaviour == BeforePrompt) $
        TextIO.putStrLn ""

    TextIO.putStr prompt
    hFlush stdout

    line <- TextIO.getLine >>= \case
        "" -> return Nothing
        x  -> return (Just x)

    when (newlineTiming behaviour == AfterPrompt) $
        TextIO.putStrLn ""

    return line
