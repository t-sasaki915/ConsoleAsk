# ConsoleAsk
A library that might be useful for asking users for many console inputs

## Example
```haskell
import System.Console.Ask

data Inputs = Inputs
    { name              :: Text
    , age               :: Maybe Int
    , birthday          :: Date
    , emailAddress      :: EmailAddress
    , needNotifications :: Bool
    } deriving Show

askInputs :: Ask Inputs
askInputs =
    Inputs
        <$> ask         "What is your name?"                    "> "
        <*> askOptional "How old are you?"                      "> "
        <*> ask         "When is your birthday?"                "> "
        <*> ask         "What is your email address?"           "> "
        <*> askOrElse   "Do you need our update notifications?" "> " False

data EmailAddress = EmailAddress Text deriving Show

instance Askable EmailAddress where
    fromText text =
        if text =~ ("[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" :: Text)
            then Just (EmailAddress text)
            else Nothing

data Date = Date Int Int deriving Show

instance Askable Date where
    -- TODO

main :: IO ()
main = do
    inputs <- runAsk defaultBehaviour askInputs

    print inputs
```
