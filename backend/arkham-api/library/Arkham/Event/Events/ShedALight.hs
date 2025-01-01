module Arkham.Event.Events.ShedALight (shedALight, ShedALight (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype ShedALight = ShedALight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shedALight :: EventCard ShedALight
shedALight = event ShedALight Cards.shedALight

instance RunMessage ShedALight where
  runMessage msg e@(ShedALight attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      otherLocations <- select $ LocationWithDiscoverableCluesBy (be iid)
      when (notNull otherLocations) $ chooseOneToHandle iid attrs otherLocations
      doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs iid (DiscoveredClues 1)
        push PassSkillTest
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DiscoveredCluesAt lid 1)
      pure e
    _ -> ShedALight <$> liftRunMessage msg attrs
