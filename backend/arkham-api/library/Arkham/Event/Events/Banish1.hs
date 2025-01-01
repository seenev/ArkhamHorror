module Arkham.Event.Events.Banish1 (banish1, Banish1 (..)) where

import Arkham.Aspect hiding (aspect)
import Arkham.ChaosToken
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Taboo

newtype Banish1 = Banish1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1 :: EventCard Banish1
banish1 = event Banish1 Cards.banish1

instance RunMessage Banish1 where
  runMessage msg e@(Banish1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvadeMatch sid iid attrs NonEliteEnemy)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          tokens <- map (.face) <$> getSkillTestRevealedChaosTokens
          locations <- select $ LocationCanBeEnteredBy eid
          when (notNull locations) $ chooseOrRunOne iid $ targetLabels locations $ only . EnemyMove eid
          let valid =
                if tabooed TabooList21 attrs
                  then any isSymbolChaosToken tokens
                  else any (`elem` tokens) [Skull, Cultist, Tablet, ElderThing]
          when valid
            $ nextPhaseModifier #upkeep attrs eid DoesNotReadyDuringUpkeep
        _ -> error "Wrong target"
      pure e
    _ -> Banish1 <$> liftRunMessage msg attrs
