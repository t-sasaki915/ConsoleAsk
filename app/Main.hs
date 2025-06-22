module Main (main) where

import           Data.Text          (Text)
import           System.Console.Ask

data Inputs = Inputs
    { textInput1 :: Text
    , textInput2 :: Text
    , textInput3 :: Text
    } deriving Show

askInputs :: Ask Inputs
askInputs =
    Inputs
        <$> askText "What is textInput1?" "Text> " (Just "ASDF")
        <*> askText "What is textInput2?" "Text> " Nothing
        <*> askText "What is textInput3?" "Text> " Nothing

main :: IO ()
main = do
    inputs <- runAsk defaultBehaviour askInputs

    putStrLn (show inputs)
