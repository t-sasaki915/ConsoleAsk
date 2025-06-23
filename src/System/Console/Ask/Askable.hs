{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ask.Askable (Askable (..)) where

import           Data.Text (Text, unpack)
import           Text.Read (readMaybe)

class Show a => Askable a where
    fromText :: Text -> Maybe a

instance Askable Text where
    fromText = Just

instance Askable String where
    fromText = Just . unpack

instance Askable Int where
    fromText = readMaybe . unpack

instance Askable Integer where
    fromText = readMaybe . unpack

instance Askable Float where
    fromText = readMaybe . unpack

instance Askable Double where
    fromText = readMaybe . unpack
