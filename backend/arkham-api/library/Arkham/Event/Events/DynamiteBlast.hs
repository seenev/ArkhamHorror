module Arkham.Event.Events.DynamiteBlast where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude
import Arkham.Projection

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: EventCard DynamiteBlast
dynamiteBlast = event DynamiteBlast Cards.dynamiteBlast

instance RunMessage DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      currentLocation <- fieldJust InvestigatorLocation iid
      connectedLocations <- select $ AccessibleFrom $ LocationWithId currentLocation
      canDealDamage <- withoutModifier iid CannotDealDamage
      choices <- forMaybeM (currentLocation : connectedLocations) $ \location -> do
        enemies <- if canDealDamage then select (enemyAt location) else pure []
        investigators <- select $ investigatorAt location
        if null enemies && null investigators
          then pure Nothing
          else do
            animation <- uiEffect attrs location Explosion
            pure
              $ Just
                ( location
                , animation
                    : map (nonAttackEnemyDamage attrs 3) enemies
                      <> map (\iid' -> assignDamage iid' attrs 3) investigators
                )
      let availableChoices = map (uncurry targetLabel) $ filter (notNull . snd) choices
      player <- getPlayer iid
      push $ chooseOne player availableChoices
      pure e
    _ -> DynamiteBlast <$> runMessage msg attrs
