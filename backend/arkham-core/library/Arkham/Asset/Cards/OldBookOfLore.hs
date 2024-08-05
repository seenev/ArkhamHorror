module Arkham.Asset.Cards.OldBookOfLore (OldBookOfLore (..), oldBookOfLore) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = asset OldBookOfLore Cards.oldBookOfLore

instance HasAbilities OldBookOfLore where
  getAbilities (OldBookOfLore a) =
    [ (controlledAbility a 1)
        ( exists
            $ affectsOthers
            $ InvestigatorAt YourLocation
            <> InvestigatorWithoutModifier CannotManipulateDeck
        )
        (actionAbilityWithCost $ exhaust a)
    ]

instance RunMessage OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigators <- select =<< guardAffectsColocated iid
      player <- getPlayer iid
      push
        $ chooseOne player
        $ targetLabels investigators
        $ \iid' -> only $ search iid' source iid' [fromTopOfDeck 3] #any (DrawFound iid' 1)
      pure a
    _ -> OldBookOfLore <$> runMessage msg attrs
