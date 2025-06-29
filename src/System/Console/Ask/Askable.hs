{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ask.Askable
    ( Askable (..)
    , fromParsec
    , defaultToText
    ) where

import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Parsec     hiding (lower)
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

class Askable a where
    fromText :: Text -> Maybe a
    toText    :: a -> Text

defaultToText :: (Show a, Askable a) => a -> Text
defaultToText = Text.show

fromParsec :: Parsec Text () a -> Text -> Maybe a
fromParsec parser = either (const Nothing) Just . parse parser ""

instance Askable Text where
    fromText = Just

instance Askable String where
    fromText = Just . Text.pack

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
