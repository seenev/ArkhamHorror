{-# LANGUAGE TemplateHaskell #-}

module Arkham.Text where

import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH

newtype Tooltip = Tooltip Text
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord)

data FlavorText = FlavorText
  { flavorTitle :: Maybe Text
  , flavorBody :: [Text]
  }
  deriving stock (Show, Eq, Ord, Data)

i18n :: Text -> FlavorText
i18n = FlavorText Nothing . pure . toI18n

i18nWithTitle :: Text -> FlavorText
i18nWithTitle t = FlavorText (Just $ toI18n $ t <> ".title") [toI18n $ t <> ".body"]

toI18n :: Text -> Text
toI18n = ("$" <>)

instance Semigroup FlavorText where
  FlavorText mTitle1 body1 <> FlavorText mTitle2 body2 = FlavorText (mTitle1 <|> mTitle2) (body1 <> body2)

instance Monoid FlavorText where
  mempty = FlavorText Nothing []

instance IsString FlavorText where
  fromString s = FlavorText Nothing [fromString s]

$(deriveJSON (aesonOptions $ Just "flavor") ''FlavorText)
