module Arkham.Types.Asset.Cards.JimsTrumpet
  ( JimsTrumpet(..)
  , jimsTrumpet
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Token

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

jimsTrumpet :: AssetCard JimsTrumpet
jimsTrumpet = hand JimsTrumpet Cards.jimsTrumpet

instance HasAbilities JimsTrumpet where
  getAbilities (JimsTrumpet x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorExists
          (AnyInvestigator
              [InvestigatorAt YourLocation, InvestigatorAt ConnectedLocation]
          <> InvestigatorWithAnyHorror
          )
        )
        (ReactionAbility
          (RevealChaosToken Timing.When Anyone (TokenFaceIs Skull))
          (ExhaustCost $ toTarget x)
        )
    ]

instance AssetRunner env => RunMessage env JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      let ownerId = fromJustNote "must be owned" assetInvestigator
      locationId <- getId ownerId
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList locationId
      investigatorIds <-
        concat <$> for (locationId : connectedLocationIds) getSetList
      pairings <- for investigatorIds
        $ \targetId -> (targetId, ) . unHorrorCount <$> getCount targetId
      let choices = map fst $ filter ((> 0) . snd) pairings
      a <$ push
        (chooseOne
          ownerId
          [ HealHorror (InvestigatorTarget iid) 1 | iid <- choices ]
        )
    _ -> JimsTrumpet <$> runMessage msg attrs
