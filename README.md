# ConsoleAsk
A library that might be useful for asking users for many console inputs

## Example
```haskell
import Data.Text (Text)
import System.Console.Ask

data Inputs = Inputs
    { name              :: Text
    , age               :: Maybe Int
    , birthday          :: Date
    , needNotifications :: Bool
    } deriving Show

askInputs :: Ask Inputs
askInputs =
    Inputs
        <$> ask         "What is your name?"                    "> "
        <*> askOptional "How old are you?"                      "> "
        <*> ask         "When is your birthday?"                "> "
        <*> askOrElse   "Do you need our update notifications?" "> " False

data Date = Date Int Int deriving Show

instance Askable Date where
    -- TODO

main :: IO ()
main = do
    inputs <- runAsk defaultBehaviour askInputs

    print inputs
```
