module Arkham.Treachery.Cards.SpacesBetween (spacesBetween, SpacesBetween (..)) where

import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryCard SpacesBetween
spacesBetween = treachery SpacesBetween Cards.spacesBetween

instance RunMessage SpacesBetween where
  runMessage msg t@(SpacesBetween attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      nonSentinelHillLocations <- select $ LocationWithoutTrait SentinelHill
      msgs <- flip concatMapM' nonSentinelHillLocations \flipLocation -> do
        let locationMatcher = LocationWithId flipLocation
        investigatorIds <- select $ InvestigatorAt locationMatcher
        enemyIds <- select $ EnemyAt locationMatcher <> UnengagedEnemy
        destination <-
          fromJustNote "must be connected to a sentinel location"
            <$> selectOne (ConnectedTo locationMatcher <> LocationWithTrait SentinelHill)

        pure
          $ [Move $ move source iid destination | iid <- investigatorIds]
          <> [EnemyMove eid destination | eid <- enemyIds]
          <> [RemoveAllClues (toSource attrs) (toTarget flipLocation), UnrevealLocation flipLocation]

      alteredPaths <-
        shuffleM
          =<< filterM (fieldP LocationUnrevealedName (== "Altered Path")) nonSentinelHillLocations
      divergingPaths <-
        shuffleM
          =<< filterM (fieldP LocationUnrevealedName (== "Diverging Path")) nonSentinelHillLocations

      pushAll
        $ msgs
        <> [ SetLocationLabel locationId $ "alteredPath" <> tshow idx
           | (idx, locationId) <- withIndex1 alteredPaths
           ]
        <> [ SetLocationLabel locationId $ "divergingPath" <> tshow idx
           | (idx, locationId) <- withIndex1 divergingPaths
           ]
      pure t
    _ -> SpacesBetween <$> runMessage msg attrs
