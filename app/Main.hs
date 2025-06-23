module Main (main) where

import           Data.Text          (Text)
import           System.Console.Ask

data Inputs = Inputs
    { textInput1   :: Text
    , textInput2   :: Text
    , stringInput2 :: Text
    , intInput1    :: Int
    , boolInput1   :: Bool
    } deriving Show

askInputs :: Ask Inputs
askInputs =
    Inputs
        <$> askOrElse "What is textInput1?"   "Text> " "ASDF"
        <*> ask       "What is textInput2?"   "Text> "
        <*> ask       "What is stringInput1?" "String> "
        <*> ask       "What is intInput1?"    "Int> "
        <*> askOrElse "What is boolInput1?"   "Y/N>"   True

main :: IO ()
main = do
    inputs <- runAsk defaultBehaviour askInputs

    print inputs
