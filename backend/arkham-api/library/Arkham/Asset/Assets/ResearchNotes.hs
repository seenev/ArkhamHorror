module Arkham.Asset.Assets.ResearchNotes (researchNotes, ResearchNotes (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (getCanDiscoverClues, withLocationOf)
import Arkham.Helpers.Window (placedTokens)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message (getChoiceAmount)
import Arkham.Taboo

newtype ResearchNotes = ResearchNotes AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchNotes :: AssetCard ResearchNotes
researchNotes = asset ResearchNotes Cards.researchNotes

instance HasAbilities ResearchNotes where
  getAbilities (ResearchNotes a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ InvestigatorPlacedFromTheirPool
          #after
          You
          SourceIsPlayerCard
          (LocationTargetMatches YourLocation)
          Clue
    , skillTestAbility $ restrictedAbility a 2 ControlsThis actionAbility
    ]

instance RunMessage ResearchNotes where
  runMessage msg a@(ResearchNotes attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (placedTokens Clue -> n) _ -> do
      placeTokens (attrs.ability 1) attrs Evidence n
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 0)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 2 -> True) n | n > 0 -> do
      let spendable = min n (attrs.use Evidence)
      when (spendable > 0) do
        withLocationOf iid \lid -> do
          whenM (getCanDiscoverClues NotInvestigate iid lid) do
            if tabooed TabooList21 attrs
              then whenNotAtMax Cards.researchNotes 3 \remaining -> do
                chooseAmount iid "Evidence" "Evidence" 0 (min remaining spendable) attrs
              else chooseAmount iid "Evidence" "Evidence" 0 spendable attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Evidence" -> n) (isTarget attrs -> True) | n > 0 -> do
      when (tabooed TabooList21 attrs) do
        updateMax Cards.researchNotes n #round
      push $ SpendUses (attrs.ability 2) (toTarget attrs) Evidence n
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) n
      pure a
    _ -> ResearchNotes <$> liftRunMessage msg attrs
