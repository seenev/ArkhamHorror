module Arkham.Act.Cards.OpeningTheMaw (
  OpeningTheMaw (..),
  openingTheMaw,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype OpeningTheMaw = OpeningTheMaw ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openingTheMaw :: ActCard OpeningTheMaw
openingTheMaw = act (2, A) OpeningTheMaw Cards.openingTheMaw Nothing

instance HasAbilities OpeningTheMaw where
  getAbilities (OpeningTheMaw a) =
    if onSide A a
      then
        [ mkAbility a 1
            $ ActionAbility []
            $ ActionCost 1
            <> GroupClueCost
              (PerPlayer 1)
              (locationIs Locations.mouthOfKnYanTheCavernsMaw)
        , restrictedAbility
            a
            2
            ( ResourcesOnLocation
                (locationIs Locations.mouthOfKnYanTheCavernsMaw)
                (AtLeast (Static 6))
            )
            $ ForcedAbility AnyWindow
        ]
      else []

instance RunMessage OpeningTheMaw where
  runMessage msg a@(OpeningTheMaw attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mouthOfKnYan <-
        selectJust
          $ locationIs Locations.mouthOfKnYanTheCavernsMaw
      push $ PlaceResources (toAbilitySource attrs 1) (toTarget mouthOfKnYan) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ scenarioResolution 1
      pure a
    _ -> OpeningTheMaw <$> runMessage msg attrs
