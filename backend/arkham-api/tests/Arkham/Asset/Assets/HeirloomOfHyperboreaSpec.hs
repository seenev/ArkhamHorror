module Arkham.Asset.Assets.HeirloomOfHyperboreaSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Heirloom of Hyperborea" $ do
  context "reaction" $ do
    it "causes you to draw a card" . gameTest $ \self -> do
      self `loadDeck` [Assets.flashlight, Assets.flashlight]
      self `putCardIntoPlay` Assets.heirloomOfHyperborea
      shrivelling <- genCard Assets.shrivelling
      withProp @"resources" 3 self
      withProp @"hand" [shrivelling] self
      self `playCard` shrivelling
      useReaction
      asDefs self.hand `shouldReturn` [Assets.flashlight]
