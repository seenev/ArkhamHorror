module Arkham.Types.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs =
  location AlchemyLabs Cards.alchemyLabs 5 (Static 0) Squiggle [Hourglass]

instance HasModifiersFor env AlchemyLabs where
  getModifiersFor _ target (AlchemyLabs attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AlchemyLabs where
  getAbilities (AlchemyLabs attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
        $ ActionAbility (Just Action.Investigate) (ActionCost 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 1)
        Nothing
        SkillIntellect
        False
      )
    Successful (Action.Investigate, _) iid (AbilitySource source 1) _
      | isSource attrs source -> do
        maid <- selectOne (assetIs Cards.alchemicalConcoction)
        l <$ case maid of
          Just aid -> push (TakeControlOfAsset iid aid)
          Nothing -> pure ()
    _ -> AlchemyLabs <$> runMessage msg attrs
