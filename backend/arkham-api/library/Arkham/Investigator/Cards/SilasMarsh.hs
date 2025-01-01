module Arkham.Investigator.Cards.SilasMarsh (silasMarsh, SilasMarsh (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.SkillTest (getIsCommittable, withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype SilasMarsh = SilasMarsh InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

silasMarsh :: InvestigatorCard SilasMarsh
silasMarsh =
  investigator SilasMarsh Cards.silasMarsh
    $ Stats {health = 9, sanity = 5, willpower = 2, intellect = 2, combat = 4, agility = 4}

instance HasAbilities SilasMarsh where
  getAbilities (SilasMarsh attrs) =
    [ playerLimit PerRound
        $ restricted
          attrs
          1
          (Self <> DuringSkillTest (YourSkillTest AnySkillTest) <> exists (SkillControlledBy You))
        $ freeReaction
        $ RevealChaosToken #after You AnyChaosToken
    ]

instance HasChaosTokenValue SilasMarsh where
  getChaosTokenValue iid ElderSign (SilasMarsh attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SilasMarsh where
  runMessage msg i@(SilasMarsh attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skills <- select $ skillControlledBy iid
      chooseOrRunOneM iid $ targets skills $ returnToHand iid
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      skills <-
        filterM (getIsCommittable iid)
          . filter (`cardMatch` CardWithType SkillType)
          =<< fieldMap InvestigatorDiscard (map toCard) iid

      when (notNull skills) do
        withSkillTest \sid -> do
          focusCards skills \unfocus -> do
            chooseOneM iid do
              labeled "Do not commit skills" nothing
              targets skills \card -> do
                push $ CommitCard iid card
                skillTestModifier sid ElderSign card ReturnToHandAfterTest
            push unfocus
      pure i
    _ -> SilasMarsh <$> liftRunMessage msg attrs
