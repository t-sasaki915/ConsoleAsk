# ConsoleAsk
Simple CLI user input library

## Example
```haskell
import Data.Functor ((<&>))
import Data.Text (Text)
import Text.Parsec (char, digit, many1)
import Text.Regex.TDFA ((=~))

import System.Console.Ask

data UserInformation = UserInformation
    { name              :: Text
    , age               :: Maybe Int
    , birthday          :: Date
    , emailAddress      :: EmailAddress
    , needNotifications :: Bool
    } deriving Show

askUserInformation :: Ask UserInformation
askUserInformation =
    UserInformation
        <$> ask         "What is your name?"                    "> "
        <*> askOptional "How old are you?"                      "> "
        <*> ask         "When is your birthday?"                "> "
        <*> ask         "What is your email address?"           "> "
        <*> askOrElse   "Do you need our update notifications?" "> " False

newtype EmailAddress = EmailAddress Text deriving Show

instance Askable EmailAddress where
    fromText text =
        if text =~ ("[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" :: Text)
            then Just (EmailAddress text)
            else Nothing

data Date = Date Int Int deriving Show

instance Askable Date where
    fromText = fromParsec $ do
        day   <- many1 digit <&> read
        _     <- char '/'
        month <- many1 digit <&> read

        pure (Date day month)

main :: IO ()
main = do
    userInfo <- runAsk defaultBehaviour askUserInformation

    print userInfo
```
```
What is your name?
> Toma Sasaki

How old are you?
>

When is your birthday?
> 15/9

What is your email address?
> me@t-sasaki.net

Do you need our update notifications?
Default: False
> aye

UserInformation {name = "Toma Sasaki", age = Nothing, birthday = Date 15 9, emailAddress = EmailAddress "me@t-sasaki.net", needNotifications = True}
```