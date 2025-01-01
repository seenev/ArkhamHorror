module Arkham.Act.Cards.MysteriousGateway where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement

newtype MysteriousGateway = MysteriousGateway ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousGateway :: ActCard MysteriousGateway
mysteriousGateway =
  act
    (1, A)
    MysteriousGateway
    Cards.mysteriousGateway
    (Just $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Guest Hall"))

instance RunMessage MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      lead <- getLeadPlayer
      investigatorIds <- select $ InvestigatorAt $ LocationWithTitle "Guest Hall"
      (holeInTheWallId, placeHoleInTheWall) <- placeSetAsideLocation Locations.holeInTheWall
      sid <- getRandom
      pushAll
        [ placeHoleInTheWall
        , chooseOne
            lead
            [ targetLabel
              iid'
              [ Move $ move (toSource attrs) iid' holeInTheWallId
              , beginSkillTest sid iid' (ActSource aid) iid' #willpower (Fixed 4)
              ]
            | iid' <- investigatorIds
            ]
        , AdvanceActDeck actDeckId (toSource attrs)
        ]
      pure a
    FailedSkillTest iid _ (ActSource aid) SkillTestInitiatorTarget {} _ n | aid == actId -> do
      pushAll $ replicate n $ toMessage $ randomDiscard iid attrs
      pure a
    _ -> MysteriousGateway <$> runMessage msg attrs
