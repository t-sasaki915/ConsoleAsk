{-|
Module      : System.Console.Ask.Askable
Copyright   : 2025 Toma Sasaki
Licence     : MIT
Maintainer  : netst915@gmail.com
Portability : Portable

@System.Console.Ask.Askable@ provides 'Askable' and 'fromParsec'.
You should import this module if you want to do 'System.Console.Ask.ask' for your original data types.
-}

{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ask.Askable
    ( Askable (..)
    , fromParsec
    ) where

import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Parsec     (Parsec, anyChar, eof, parse)
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

{-|
'Askable' provides 'fromText'.
Instances of 'Askable' must derive @Show@.
This typeclass is used when 'System.Console.Ask.ask', 'System.Console.Ask.askOptional' and 'System.Console.Ask.askOrElse' try to parse user input.
Implementing 'fromText' is required.
-}
class Show a => Askable a where
    {-|
    'fromText' converts @Text@ into a certain type.
    Returning @Nothing@ makes the conversion failed.

    @
    data EmailAddress = EmailAddress Text deriving Show

    instance 'Askable' EmailAddress where
        'fromText' text =
            if text =~ ("[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" :: Text)
                then Just (EmailAddress text)
                else Nothing
    @
    -}
    fromText :: Text -> Maybe a

{-|
'fromParsec' converts @Parsec Text () a@ into @Text -> Maybe a@.
It is supposed to be used with 'fromText'.

@
instance 'Askable' 'Char' where
    'fromText' = 'fromParsec' (anyChar <* eof)
@
-}
fromParsec :: Parsec Text () a -> Text -> Maybe a
fromParsec parser = either (const Nothing) Just . parse parser ""

instance Askable Text where
    fromText = Just

instance Askable String where
    fromText = Just . Text.unpack

instance Askable Int where
    fromText = readMaybe . Text.unpack

instance Askable Integer where
    fromText = readMaybe . Text.unpack

instance Askable Float where
    fromText = readMaybe . Text.unpack

instance Askable Double where
    fromText = readMaybe . Text.unpack

instance Askable Char where
    fromText = fromParsec (anyChar <* eof)

instance Askable Bool where
    fromText text =
        let lower = Text.toLower text in
            if lower =~ ("^(t(rue)?|y(es|eah)?|aye)$" :: Text)
                then Just True
                else
                    if lower =~ ("^(f(alse)?|n(o|ae)?)$" :: Text)
                        then Just False
                        else Nothing
