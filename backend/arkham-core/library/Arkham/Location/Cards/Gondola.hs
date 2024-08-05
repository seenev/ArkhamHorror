module Arkham.Location.Cards.Gondola (
  gondola,
  Gondola (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Data.List qualified as List

newtype Gondola = Gondola LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gondola :: LocationCard Gondola
gondola = location Gondola Cards.gondola 5 (Static 0)

instance HasAbilities Gondola where
  getAbilities (Gondola x) =
    withBaseAbilities
      x
      [ skillTestAbility $ restrictedAbility x 1 Here $ ActionAbility [] $ ActionCost 1
      | locationRevealed x
      ]

instance RunMessage Gondola where
  runMessage msg l@(Gondola attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      locationIds <- List.delete (toId attrs) <$> select Anywhere
      pushAll
        $ MoveAllTo (toSource attrs) (toId attrs)
        : [RemoveLocation lid | lid <- locationIds]
      pure l
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ Label "Test {combat} (2)" [beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)]
          , Label "Test {agility} (2)" [beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 2)]
          ]
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _ | isAbilitySource attrs 1 source -> do
      push (PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1)
      pure l
    _ -> Gondola <$> runMessage msg attrs
