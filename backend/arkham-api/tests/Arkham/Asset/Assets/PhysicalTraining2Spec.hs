module Arkham.Asset.Assets.PhysicalTraining2Spec (
  spec,
) where

import TestImport

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Token

spec :: Spec
spec = describe "Physical Training (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ gameTest $ \investigator -> do
    updateInvestigator investigator $ \attrs ->
      attrs {investigatorWillpower = 1, investigatorTokens = setTokens Resource 2 mempty}
    putCardIntoPlay investigator Assets.physicalTraining2

    didPassTest <- didPassSkillTestBy investigator SkillWillpower 0

    pushAndRun $ SetChaosTokens [Zero]
    sid <- getRandom
    pushAndRun $ beginSkillTest sid investigator SkillWillpower 3
    chooseOptionMatching
      "use ability"
      ( \case
          AbilityLabel {ability} -> abilityIndex ability == 1
          _ -> False
      )
    chooseOptionMatching
      "use ability"
      ( \case
          AbilityLabel {ability} -> abilityIndex ability == 1
          _ -> False
      )
    chooseOptionMatching
      "start skill test"
      ( \case
          StartSkillTestButton {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didPassTest `refShouldBe` True

  it "Adds 1 to combat check for each resource spent" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ \attrs -> attrs {investigatorCombat = 1, investigatorTokens = setTokens Resource 2 mempty}
    putCardIntoPlay investigator Assets.physicalTraining2

    didPassTest <- didPassSkillTestBy investigator SkillCombat 0

    pushAndRun $ SetChaosTokens [Zero]
    sid <- getRandom
    pushAndRun $ beginSkillTest sid investigator SkillCombat 3
    chooseOptionMatching
      "use ability"
      ( \case
          AbilityLabel {ability} -> abilityIndex ability == 2
          _ -> False
      )
    chooseOptionMatching
      "use ability"
      ( \case
          AbilityLabel {ability} -> abilityIndex ability == 2
          _ -> False
      )
    chooseOptionMatching
      "start skill test"
      ( \case
          StartSkillTestButton {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didPassTest `refShouldBe` True
