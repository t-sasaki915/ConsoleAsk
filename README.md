# ConsoleAsk
Simple CLI user input library

## Example
```haskell
import Data.Functor ((<&>))
import Data.Text (Text)
import Text.Parsec (char, digit, many1)
import Text.Regex.TDFA ((=~))

import System.Console.Ask (Ask, ask, askOptional, askOrElse, runAsk, defaultBehaviour)
import System.Console.Ask.Askable (Askable (fromText), fromParsec)

data UserInformation = UserInformation
    { name                   :: Text
    , age                    :: Maybe Int
    , birthday               :: Date
    , notificationPreference :: NotificationPreference
    } deriving Show

data NotificationPreference = NotificationPreference
    { needNotifications :: Bool
    , emailAddress      :: Maybe EmailAddress
    } deriving Show

askUserInformation :: Ask UserInformation
askUserInformation =
    UserInformation
        <$> ask         "What is your name?"
        <*> askOptional "How old are you?"
        <*> ask         "When is your birthday?"
        <*> askNotificationPreference

askNotificationPreference :: Ask NotificationPreference
askNotificationPreference = do
    needNotifications' <- askOrElse "Do you need our update notifications?" False

    emailAddress' <-
        if needNotifications'
            then Just <$> ask "What is your email address?"
            else pure Nothing

    pure NotificationPreference
        { needNotifications = needNotifications'
        , emailAddress      = emailAddress'
        }

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

Do you need our update notifications? (Default: False)
> aye

What is your email address?
> me@t-sasaki.net

UserInformation
    { name = "Toma Sasaki"
    , age = Nothing
    , birthday = Date 15 9
    , notificationPreference =
        NotificationPreference
            { needNotifications = True
            , emailAddress = Just (EmailAddress "me@t-sasaki.net")
            }
    }
```

## Features
- Automatically parses input values to `Askable` instance types.
```haskell
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import System.Console.Ask (ask, defaultBehaviour, liftIO, runAsk)

main :: IO ()
main = runAsk defaultBehaviour $ do
    name <- ask "What is your name?" :: Ask Text
    age  <- ask "How old are you?"   :: Ask Int

    liftIO $ TextIO.putStrLn ("Name: " <> name <> ", Age: " <> Text.show age)
```
```
What is your name?
> Toma Sasaki

How old are you?
> a
Invalid input.

How old are you?
> 18

When is your birthday?
> 15/9

Name: Toma Sasaki, Age: 18, Birthday: Date 15 9
```

- `Askable` supports both `Text -> Maybe a` and parsec.
```haskell
import Data.Functor ((<&>))
import Data.Text (Text)
import Text.Parsec (char, digit, many1)
import Text.Regex.TDFA ((=~))

import System.Console.Ask.Askable (Askable (fromText), fromParsec)

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
```
