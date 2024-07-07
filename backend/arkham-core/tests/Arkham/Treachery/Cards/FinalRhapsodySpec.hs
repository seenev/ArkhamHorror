module Arkham.Treachery.Cards.FinalRhapsodySpec (
  spec,
) where

import TestImport.Lifted hiding (InvestigatorDamage)

import Arkham.Investigator.Types (Field (..))
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Final Rhapsody" $ do
  it "does 1 damage per skull and autofail revealed" $ gameTest $ \investigator -> do
    finalRhapsody <- genPlayerCard Cards.finalRhapsody
    pushAndRunAll
      [ SetChaosTokens [Skull, Skull, AutoFail, Zero, Cultist]
      , loadDeck investigator [finalRhapsody]
      , drawCards (toId investigator) investigator 1
      ]
    chooseFirstOption "acknowledge damage"
    chooseFirstOption "take damage"
    chooseFirstOption "take damage"
    chooseFirstOption "take damage"
    chooseFirstOption "take horror"
    chooseFirstOption "take horror"
    chooseFirstOption "take horror"
    fieldAssert InvestigatorDamage (== 3) investigator
    fieldAssert InvestigatorHorror (== 3) investigator
    assert $ Cards.finalRhapsody `isInDiscardOf` investigator
