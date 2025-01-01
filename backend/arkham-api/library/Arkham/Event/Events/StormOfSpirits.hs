module Arkham.Event.Events.StormOfSpirits (stormOfSpirits, stormOfSpiritsEffect, StormOfSpirits (..)) where

import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Matcher hiding (AttackDamageEffect, RevealChaosToken)
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      chooseFight <-
        leftOr
          <$> aspect iid attrs (#willpower `InsteadOf` #combat) (setTarget attrs <$> mkChooseFight sid iid attrs)
      enabled <- createCardEffect Cards.stormOfSpirits (effectMetaTarget sid) attrs iid
      pushAll $ enabled : chooseFight
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      let
        toMsg eid' =
          if eid == eid'
            then EnemyDamage eid' $ delayDamage $ attack attrs 2
            else EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 2
      eids <- select $ enemyAtLocationWith iid
      pushAll $ map toMsg eids <> map (checkDefeated attrs) eids
      pure e
    _ -> StormOfSpirits <$> runMessage msg attrs

newtype StormOfSpiritsEffect = StormOfSpiritsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpiritsEffect :: EffectArgs -> StormOfSpiritsEffect
stormOfSpiritsEffect = cardEffect StormOfSpiritsEffect Cards.stormOfSpirits

instance RunMessage StormOfSpiritsEffect where
  runMessage msg e@(StormOfSpiritsEffect attrs) = case msg of
    RevealChaosToken _ iid token | toTarget iid == effectTarget attrs -> do
      let triggers = chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when triggers $ do
        enemy <- fromJustNote "must be enemy" . ((.enemy) =<<) <$> getSkillTestTarget
        iids <- select $ InvestigatorAt $ locationWithEnemy enemy
        pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token (toId attrs))
              [assignDamage iid' (effectSource attrs) 1 | iid' <- iids]
          , DisableEffect $ toId attrs
          ]
      pure e
    SkillTestEnds _ _ _ -> do
      push (DisableEffect $ toId attrs)
      pure e
    _ -> StormOfSpiritsEffect <$> runMessage msg attrs
