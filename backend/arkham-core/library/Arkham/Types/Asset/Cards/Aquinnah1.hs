{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Aquinnah1
  ( Aquinnah1(..)
  , aquinnah1
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Aquinnah1 = Aquinnah1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aquinnah1 :: AssetId -> Aquinnah1
aquinnah1 uuid = Aquinnah1 $ (baseAttrs uuid "01082")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 4
  }

reactionAbility :: Attrs -> Ability
reactionAbility attrs =
  mkAbility (toSource attrs) 1 (FastAbility (WhenEnemyAttacks You))

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasModifiersFor env investigator Aquinnah1 where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Aquinnah1 where
  getActions i (WhenEnemyAttacks You) (Aquinnah1 a) | ownedBy a i = do
    enemyId <- fromQueue $ \queue ->
      let PerformEnemyAttack iid eid : _ = dropUntilAttack queue
      in if iid == getId () i then eid else error "mismatch"
    enemyIds <- asks $ filterSet (/= enemyId) . getSet (locationOf i)
    pure
      [ ActivateCardAbilityAction (getId () i) (reactionAbility a)
      | not (assetExhausted a) && not (null enemyIds)
      ]
  getActions i window (Aquinnah1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env Aquinnah1 where
  runMessage msg (Aquinnah1 attrs) = case msg of
    UseCardAbility iid _ source _ 1 | isSource attrs source -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- asks $ unHealthDamageCount . getCount enemyId
      sanityDamage' <- asks $ unSanityDamageCount . getCount enemyId
      locationId <- asks $ getId @LocationId iid
      enemyIds <- asks $ filter (/= enemyId) . setToList . getSet locationId

      when (null enemyIds) (error "other enemies had to be present")

      unshiftMessage $ chooseOne
        iid
        [ Run
            [ EnemyDamage eid iid source healthDamage'
            , InvestigatorAssignDamage iid (EnemySource enemyId) 0 sanityDamage'
            ]
        | eid <- enemyIds
        ]

      pure $ Aquinnah1 $ attrs & exhausted .~ True
    _ -> Aquinnah1 <$> runMessage msg attrs
