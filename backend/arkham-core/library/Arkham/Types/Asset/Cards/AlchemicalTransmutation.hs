module Arkham.Types.Asset.Cards.AlchemicalTransmutation
  ( alchemicalTransmutation
  , AlchemicalTransmutation(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype AlchemicalTransmutation = AlchemicalTransmutation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation :: AssetCard AlchemicalTransmutation
alchemicalTransmutation =
  arcane AlchemicalTransmutation Cards.alchemicalTransmutation

instance HasAbilities AlchemicalTransmutation where
  getAbilities (AlchemicalTransmutation a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ExhaustCost (toTarget a), UseCost (toId a) Charge 1]
    ]

instance AssetRunner env => RunMessage env AlchemicalTransmutation where
  runMessage msg a@(AlchemicalTransmutation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ CreateEffect "03032" Nothing source (InvestigatorTarget iid)
      , BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 1
      ]
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        a <$ push (TakeResources iid (min n 3) False)
    _ -> AlchemicalTransmutation <$> runMessage msg attrs
