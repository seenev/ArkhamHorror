module Arkham.Event.Cards.ImprovisedWeapon (improvisedWeapon, ImprovisedWeapon (..)) where

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Zone

newtype ImprovisedWeapon = ImprovisedWeapon EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedWeapon :: EventCard ImprovisedWeapon
improvisedWeapon = event ImprovisedWeapon Cards.improvisedWeapon

instance RunMessage ImprovisedWeapon where
  runMessage msg e@(ImprovisedWeapon attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      enemyIds <- select $ CanFightEnemy (toSource attrs)
      player <- getPlayer iid
      sid <- getRandom
      pushAll
        $ [skillTestModifier sid attrs iid (DamageDealt 1) | zone == FromDiscard]
        <> [ chooseOne
              player
              [ targetLabel
                enemyId
                [ skillTestModifier sid attrs enemyId (EnemyFight (-1))
                , FightEnemy sid iid enemyId (toSource attrs) Nothing SkillCombat False
                ]
              | enemyId <- enemyIds
              ]
           ]
        <> [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs) | zone == FromDiscard]
      pure e
    _ -> ImprovisedWeapon <$> runMessage msg attrs
