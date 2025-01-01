module Arkham.Helpers.Location where

import Arkham.Prelude

import Arkham.Asset.Types (AssetAttrs, Field (..))
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query hiding (matches)
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.LocationSymbol
import Arkham.Matcher hiding (LocationCard)
import Arkham.Placement
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Types (Field (..))

getConnectedLocations :: HasGame m => LocationId -> m [LocationId]
getConnectedLocations = fieldMap LocationConnectedLocations toList

toConnections :: HasGame m => LocationId -> m [LocationSymbol]
toConnections lid =
  fieldMap LocationCard (cdLocationRevealedConnections . toCardDef) lid

getConnectedMatcher :: HasGame m => LocationId -> m LocationMatcher
getConnectedMatcher l = do
  isRevealed <- field LocationRevealed l
  directionalMatchers <-
    fieldMap
      LocationConnectsTo
      (map (`LocationInDirection` self) . setToList)
      l
  base <-
    if isRevealed
      then field LocationRevealedConnectedMatchers l
      else field LocationConnectedMatchers l

  modifiers <- getModifiers (LocationTarget l)
  LocationMatchAny
    <$> foldM applyModifier (base <> directionalMatchers) modifiers
 where
  applyModifier current (ConnectedToWhen whenMatcher matcher) = do
    matches <- elem l <$> select whenMatcher
    pure $ current <> [matcher | matches]
  applyModifier current _ = pure current
  self = LocationWithId l

isAt :: (HasGame m, Entity a, EntityId a ~ LocationId) => InvestigatorId -> a -> m Bool
isAt iid (toId -> lid) = fieldMap InvestigatorLocation (elem lid) iid

placementLocation :: (HasCallStack, HasGame m) => Placement -> m (Maybe LocationId)
placementLocation = \case
  AtLocation lid -> pure $ Just lid
  AttachedToLocation lid -> pure $ Just lid
  InPlayArea iid -> field InvestigatorLocation iid
  InThreatArea iid -> field InvestigatorLocation iid
  AttachedToInvestigator iid -> field InvestigatorLocation iid
  AttachedToEnemy eid -> join <$> fieldMay EnemyLocation eid
  AttachedToTreachery tid -> field TreacheryLocation tid
  AttachedToAsset aid' _ -> field AssetLocation aid'
  InVehicle aid' -> field AssetLocation aid'
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  Unplaced -> pure Nothing
  Global -> pure Nothing
  Limbo -> pure Nothing
  OutOfPlay _ -> pure Nothing
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AsSwarm eid _ -> field EnemyLocation eid
  HiddenInHand _ -> pure Nothing
  OnTopOfDeck _ -> pure Nothing
  NextToAgenda -> pure Nothing
  Near _ -> pure Nothing

class Locateable a where
  getLocationOf :: HasGame m => a -> m (Maybe LocationId)

instance Locateable InvestigatorId where
  getLocationOf = field InvestigatorLocation

instance Locateable EnemyId where
  getLocationOf = field EnemyLocation

instance Locateable AssetId where
  getLocationOf = field AssetPlacement >=> placementLocation

instance Locateable AssetAttrs where
  getLocationOf = getLocationOf . toId

instance Locateable Placement where
  getLocationOf = placementLocation

onSameLocation :: (HasGame m, Locateable a, Locateable b) => a -> b -> m Bool
onSameLocation a b = do
  mlid1 <- getLocationOf a
  mlid2 <- getLocationOf b
  pure $ case (mlid1, mlid2) of
    (Just l1, Just l2) -> l1 == l2
    _ -> False
