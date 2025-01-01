module Arkham.Treachery.Cards.AbandonedAndAloneSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Abandoned and Alone" $ do
  context "Revelation" $ do
    it "deals 2 direct horror and removes all discards from the game" . gameTest $ \self -> do
      withPropM @"discard" (genPlayerCards [Assets.flashlight, Assets.flashlight]) self
      self `putCardIntoPlay` Assets.elderSignAmulet3
      self `drawsCard` Treacheries.abandonedAndAlone
      applyAllHorror -- since direct, no other choice than to apply to self
      self.horror `shouldReturn` 2
      asDefs self.discard `shouldReturn` [Treacheries.abandonedAndAlone]
