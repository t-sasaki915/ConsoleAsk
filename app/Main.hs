module Main (main) where

import           Data.Text          (Text)
import           System.Console.Ask

data Inputs = Inputs
    { textInput1 :: Text
    , textInput2 :: Text
    , textInput3 :: Int
    } deriving Show

askInputs :: Ask Inputs
askInputs =
    Inputs
        <$> askOrElse "What is textInput1?" "Text> " "ASDF"
        <*> ask       "What is textInput2?" "Text> "
        <*> ask       "What is textInput3?" "Text> "

main :: IO ()
main = do
    inputs <- runAsk defaultBehaviour askInputs

    putStrLn (show inputs)
