{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ask.Askable
    ( Askable (..)
    , fromParsec
    , toParsec
    ) where

import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Parsec     hiding (lower)
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

class Show a => Askable a where
    fromText :: Text -> Maybe a

fromParsec :: Parsec Text () a -> Text -> Maybe a
fromParsec parser = either (const Nothing) Just . parse parser ""

toParsec :: (Text -> Maybe a) -> Parsec Text () a
toParsec f = getInput >>= maybe (fail "parse error") pure . f

instance Askable Text where
    fromText = Just

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

instance Askable a => Askable [a] where
    fromText = fromParsec $ toParsec fromText <> many (char ',' *> toParsec fromText)

instance Askable String where
    fromText = Just . Text.unpack

instance Askable Bool where
    fromText text =
        let lower = Text.toLower text in
            if lower =~ ("^(t(rue)?|y(es)?|aye)$" :: Text)
                then Just True
                else
                    if lower =~ ("^(f(alse)?|n(o|ae)?)$" :: Text)
                        then Just False
                        else Nothing
