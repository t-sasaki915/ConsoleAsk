{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ask.Askable (Askable (..)) where

import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

class Show a => Askable a where
    fromText :: Text -> Maybe a

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

instance Askable Bool where
    fromText text =
        let lower = Text.toLower text in
            if lower =~ ("^(t(rue)?|y(es)?|aye)$" :: Text)
                then Just True
                else
                    if lower =~ ("^(f(alse)?|n(o|ae)?)$" :: Text)
                        then Just False
                        else Nothing
