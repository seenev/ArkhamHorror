module Arkham.Types.Act.Cards.SkinGame
  ( SkinGame(..)
  , skinGame
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

skinGame :: ActCard SkinGame
skinGame = act
  (2, A)
  SkinGame
  Cards.skinGame
  (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "VIP Area"))

instance ActRunner env => RunMessage env SkinGame where
  runMessage msg a@(SkinGame attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      peterClover <- EncounterCard <$> genEncounterCard Assets.peterClover
      drFrancisMorgan <- PlayerCard <$> genPlayerCard Assets.drFrancisMorgan
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      vipAreaId <- getJustLocationIdByName "VIP Area"
      a <$ if completedExtracurricularActivity
        then pushAll
          [ CreateStoryAssetAt peterClover cloverClubBarId
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardWithType EnemyType <> CardWithTrait Abomination)
          , NextAct (toId attrs) "02069"
          ]
        else pushAll
          [ CreateStoryAssetAt drFrancisMorgan vipAreaId
          , NextAct (toId attrs) "02068"
          ]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      a <$ push (SpawnEnemyAt (EncounterCard ec) cloverClubBarId)
    _ -> SkinGame <$> runMessage msg attrs
