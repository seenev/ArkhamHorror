module Arkham.Treachery.Cards.TheCreaturesTracks (
  theCreaturesTracks,
  TheCreaturesTracks (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheCreaturesTracks = TheCreaturesTracks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCreaturesTracks :: TreacheryCard TheCreaturesTracks
theCreaturesTracks = treachery TheCreaturesTracks Cards.theCreaturesTracks

instance RunMessage TheCreaturesTracks where
  runMessage msg t@(TheCreaturesTracks attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      anyBroodOfYogSothoth <- selectAny $ SetAsideCardMatch $ CardWithTitle broodTitle
      if anyBroodOfYogSothoth
        then do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label
                  "Take 2 horror"
                  [InvestigatorAssignDamage iid source DamageAny 0 2]
              , Label
                  "Spawn a set aside Brood of Yog-Sothoth at a random location"
                  [ChooseRandomLocation (toTarget attrs) mempty]
              ]
        else push $ InvestigatorAssignDamage iid source DamageAny 0 2
      pure t
    ChosenRandomLocation target lid | isTarget attrs target -> do
      setAsideBroodOfYogSothoth <- getSetAsideBroodOfYogSothoth
      for_ (nonEmpty setAsideBroodOfYogSothoth) $ \xs -> do
        x <- sample xs
        pushM $ createEnemyAt_ x lid Nothing
      pure t
    _ -> TheCreaturesTracks <$> runMessage msg attrs
