module Arkham.Helpers.Slot (
  module Arkham.Helpers.Slot,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Slot as X

isEmptySlot :: Slot -> Bool
isEmptySlot = null . slotItems

canPutIntoSlot :: (HasGame m, IsCard a) => a -> Slot -> m Bool
canPutIntoSlot a = \case
  slot@(Slot _ []) -> pure $ isEmptySlot slot
  slot@(AdjustableSlot _ restriction _ []) -> pure $ isEmptySlot slot && maybe True (cardMatch a) restriction
  tslot@(RestrictedSlot _ matcher []) -> pure $ isEmptySlot tslot && cardMatch a matcher
  Slot _ (x : xs) -> do
    mods <- getModifiers x
    let canFit = \case
          SharesSlotWith n matcher -> length xs + 1 < n && cardMatch a matcher
          _ -> False
    pure $ any canFit mods
  RestrictedSlot _ matcher (x : xs) -> do
    mods <- getModifiers x
    let canFit = \case
          SharesSlotWith n matcher' -> length xs + 1 < n && cardMatch a matcher'
          _ -> False
    pure $ any canFit mods && cardMatch a matcher
  AdjustableSlot _ restriction _ (x : xs) -> do
    mods <- getModifiers x
    let canFit = \case
          SharesSlotWith n matcher -> length xs + 1 < n && cardMatch a matcher
          _ -> False
    pure $ any canFit mods && maybe True (cardMatch a) restriction

putIntoSlot :: AssetId -> Slot -> Slot
putIntoSlot aid = \case
  Slot source assets -> Slot source (aid : assets)
  RestrictedSlot source t assets -> RestrictedSlot source t (aid : assets)
  AdjustableSlot source restriction t assets -> AdjustableSlot source restriction t (aid : assets)

emptySlot :: Slot -> Slot
emptySlot = \case
  Slot source _ -> Slot source []
  RestrictedSlot source t _ -> RestrictedSlot source t []
  AdjustableSlot source restriction t _ -> AdjustableSlot source restriction t []

slotItems :: Slot -> [AssetId]
slotItems = \case
  Slot _ assets -> assets
  RestrictedSlot _ _ assets -> assets
  AdjustableSlot _ _ _ assets -> assets

removeIfMatches :: AssetId -> Slot -> Slot
removeIfMatches aid = \case
  Slot source assets -> Slot source (filter (/= aid) assets)
  RestrictedSlot source trait assets -> RestrictedSlot source trait (filter (/= aid) assets)
  AdjustableSlot source restriction trait assets -> AdjustableSlot source restriction trait (filter (/= aid) assets)

removeIfMatchesOnce :: AssetId -> Slot -> Slot
removeIfMatchesOnce aid = \case
  Slot source assets -> Slot source (deleteFirst aid assets)
  RestrictedSlot source trait assets -> RestrictedSlot source trait (deleteFirst aid assets)
  AdjustableSlot source restriction trait assets -> AdjustableSlot source restriction trait (deleteFirst aid assets)
