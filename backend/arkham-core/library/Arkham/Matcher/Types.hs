{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Types where

import Arkham.Prelude

import Arkham.Act.Sequence (ActSide)
import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Sequence
import Arkham.Aspect.Types
import Arkham.Asset.Uses
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.ChaosToken
import Arkham.ChaosToken qualified as ChaosToken
import Arkham.ClassSymbol
import Arkham.Cost.Status
import {-# SOURCE #-} Arkham.Criteria
import Arkham.Criteria.Override
import Arkham.Damage
import Arkham.Deck
import Arkham.Direction
import Arkham.EncounterSet (EncounterSet)
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Key
import Arkham.Keyword (Keyword)
import Arkham.Label
import Arkham.Location.Brazier
import Arkham.LocationSymbol
import Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Step
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Timing
import Arkham.Trait
import Arkham.Trait qualified as Trait
import Arkham.Zone
import Control.Lens.Plated (Plated)
import Data.Aeson.TH
import GHC.OverloadedLabels

type Who = InvestigatorMatcher

pattern MostClues :: InvestigatorMatcher
pattern MostClues <- MostToken Clue
  where
    MostClues = MostToken Clue

pattern MostHorror :: InvestigatorMatcher
pattern MostHorror <- MostToken Horror
  where
    MostHorror = MostToken Horror

data InvestigatorMatcher
  = InvestigatorAt LocationMatcher
  | InvestigatorIs CardCode
  | InvestigatorCanBeAssignedDamageBy InvestigatorId
  | InvestigatorCanBeAssignedHorrorBy InvestigatorId
  | InvestigatorCanMoveTo Source LocationMatcher
  | InvestigatorWithToken Token
  | You
  | ThatInvestigator
  | UnengagedInvestigator
  | NoOne
  | NotYou
  | Anyone
  | FewestCardsInHand
  | MostCardsInHand
  | MostDamage
  | LowestRemainingHealth
  | LowestRemainingSanity
  | MostRemainingSanity
  | NearestToEnemy EnemyMatcher
  | NearestToLocation LocationMatcher
  | HasMostMatchingAsset AssetMatcher
  | HasMatchingAsset AssetMatcher
  | OwnsAsset AssetMatcher
  | HasMatchingEvent EventMatcher
  | HasMatchingSkill SkillMatcher
  | HasMatchingTreachery TreacheryMatcher
  | InvestigatorWithCommittableCard
  | MostToken Token
  | HasTokens Token ValueMatcher
  | MostKeys
  | InvestigatorWithUnhealedHorror
  | UneliminatedInvestigator
  | ResignedInvestigator
  | DefeatedInvestigator
  | ContributedMatchingIcons ValueMatcher
  | DeckWith CardListMatcher
  | HandWith CardListMatcher
  | DiscardWith CardListMatcher
  | InvestigatorWithTrait Trait
  | InvestigatorWithoutModifier ModifierType
  | InvestigatorWithModifier ModifierType
  | InvestigatorEngagedWith EnemyMatcher
  | InvestigatorWithActionsRemaining ValueMatcher
  | InvestigatorWithClues ValueMatcher
  | InvestigatorWithDamage ValueMatcher
  | InvestigatorHasCardWithDamage
  | InvestigatorHasCardWithHorror
  | InvestigatorWithDoom ValueMatcher
  | InvestigatorWithHorror ValueMatcher
  | InvestigatorWithHealableHorror
  | InvestigatorWithRemainingSanity ValueMatcher
  | InvestigatorWithResources ValueMatcher
  | InvestigatorWithSpendableResources ValueMatcher
  | InvestigatorWithId InvestigatorId
  | InvestigatorWithTreacheryInHand TreacheryMatcher
  | InvestigatorWithTitle Text
  | InvestigatorMatches [InvestigatorMatcher]
  | InvestigatorWithLowestSkill SkillType
  | InvestigatorWithHighestSkill SkillType
  | AnyInvestigator [InvestigatorMatcher]
  | TurnInvestigator
  | ActiveInvestigator
  | LeadInvestigator
  | NoDamageDealtThisTurn
  | NoSuccessfulExploreThisTurn
  | TopCardOfDeckIs CardMatcher
  | YetToTakeTurn
  | NotInvestigator InvestigatorMatcher
  | InvestigatorThatMovedDuringTurn
  | InvestigatorWithSupply Supply
  | InvestigatorCanDiscoverCluesAtOneOf LocationMatcher -- NOTE: Use matcher above
  | DeckIsEmpty
  | AliveInvestigator
  | IncludeEliminated InvestigatorMatcher
  | HealableInvestigator Source DamageType InvestigatorMatcher
  | InvestigatorWithMostCardsInPlayArea
  | InvestigatorWithClass ClassSymbol
  | InvestigatorWithKey ArkhamKey
  | InvestigatorWithBondedCard CardMatcher
  | InvestigatorIfThen InvestigatorMatcher InvestigatorMatcher InvestigatorMatcher
  | InvestigatorCanTarget Target
  | InvestigatorWithRecord CampaignLogKey
  | CanBeHuntedBy EnemyId
  | DistanceFromRoundStart ValueMatcher
  | InvestigatorWithMetaKey Text
  | InvestigatorWithFilledSlot SlotType
  | InvestigatorWithAnyFailedSkillTestsThisTurn
  deriving stock (Show, Eq, Ord, Data)

instance Plated InvestigatorMatcher

instance Not InvestigatorMatcher where
  not_ = NotInvestigator

instance Semigroup InvestigatorMatcher where
  Anyone <> x = x
  x <> Anyone = x
  InvestigatorMatches xs <> InvestigatorMatches ys =
    InvestigatorMatches $ xs <> ys
  InvestigatorMatches xs <> x = InvestigatorMatches (x : xs)
  x <> InvestigatorMatches xs = InvestigatorMatches (x : xs)
  x <> y = InvestigatorMatches [x, y]

instance Monoid InvestigatorMatcher where
  mempty = Anyone

data PreyMatcher
  = Prey InvestigatorMatcher
  | OnlyPrey InvestigatorMatcher
  | BearerOf EnemyId
  | RestrictedBearerOf EnemyId InvestigatorMatcher
  deriving stock (Show, Eq, Ord, Data)

pattern AssetCanHaveUses :: UseType -> AssetMatcher
pattern AssetCanHaveUses uType <-
  AssetOneOf [AssetMatches [AssetWithUseType uType, AssetNotAtUseLimit], AssetWithoutUses]
  where
    AssetCanHaveUses uType = AssetOneOf [AssetMatches [AssetWithUseType uType, AssetNotAtUseLimit], AssetWithoutUses]

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetAttachedToAsset AssetMatcher
  | AssetWithAttachedEvent EventMatcher
  | AssetAttachedTo TargetMatcher
  | AssetControlledBy InvestigatorMatcher
  | AssetInPlayAreaOf InvestigatorMatcher
  | AssetOwnedBy InvestigatorMatcher
  | UnownedAsset
  | AssetMatches [AssetMatcher]
  | AssetOneOf [AssetMatcher]
  | AssetAtLocation LocationId
  | AssetNonStory
  | AssetReady
  | AssetExhausted
  | AssetCanLeavePlayByNormalMeans
  | AssetWithModifier ModifierType
  | AssetWithoutModifier ModifierType
  | AssetNotAtUseLimit
  | AssetNotAtUsesX
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetWithoutUses
  | AssetWithUseCount UseType ValueMatcher
  | AssetWithDoom ValueMatcher
  | AssetWithClues ValueMatcher
  | AssetWithTokens ValueMatcher Token
  | AssetWithSpendableUses ValueMatcher UseType
  | AssetWithHighestPrintedCost AssetMatcher
  | AssetWithSealedChaosTokens Int ChaosTokenMatcher
  | AssetWithoutSealedTokens
  | AssetInSlot SlotType
  | AssetInTwoHandSlots
  | AssetInSingleHand
  | AssetIs CardCode
  | AssetWithCardId CardId
  | AssetCardMatch CardMatcher
  | AnyAsset
  | NotAsset AssetMatcher
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
  | AssetWithHorror
  | AssetWithHealth
  | AssetWithSanity
  | AssetWithAnyRemainingHealth
  | AssetWithAnyRemainingSanity
  | AssetWithFewestClues AssetMatcher
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeDamagedBySource Source
  | AssetCanBeAssignedHorrorBy InvestigatorId
  | AssetWithCardsUnderneath CardListMatcher
  | ClosestAsset LocationId AssetMatcher
  | NonWeaknessAsset
  | AssetWithMatchingSkillTestIcon
  | UniqueAsset
  | PermanentAsset
  | AssetWithDifferentTitleFromAtLeastOneCardInHand InvestigatorMatcher ExtendedCardMatcher AssetMatcher
  | HealableAsset Source DamageType AssetMatcher
  | AssetWithPlacement Placement
  | AssetWithPerformableAbility AbilityMatcher [ModifierType]
  | AssetWithPerformableAbilityBy InvestigatorMatcher AbilityMatcher [ModifierType]
  deriving stock (Show, Eq, Ord, Data)

asset_ :: AssetMatcher -> AssetMatcher
asset_ = id

instance Not AssetMatcher where
  not_ = NotAsset

instance IsString AssetMatcher where
  fromString = AssetWithTitle . fromString

instance IsLabel "ally" AssetMatcher where
  fromLabel = AssetWithTrait Ally

instance IsLabel "melee" AssetMatcher where
  fromLabel = AssetWithTrait Melee

instance IsLabel "firearm" AssetMatcher where
  fromLabel = AssetWithTrait Firearm

instance IsLabel "weapon" AssetMatcher where
  fromLabel = AssetWithTrait Weapon

instance IsLabel "ranged" AssetMatcher where
  fromLabel = AssetWithTrait Ranged

instance IsLabel "tome" AssetMatcher where
  fromLabel = AssetWithTrait Tome

instance IsLabel "tool" AssetMatcher where
  fromLabel = AssetWithTrait Tool

instance IsLabel "spell" AssetMatcher where
  fromLabel = AssetWithTrait Spell

instance IsLabel "science" AssetMatcher where
  fromLabel = AssetWithTrait Science

instance IsLabel "ritual" AssetMatcher where
  fromLabel = AssetWithTrait Ritual

instance IsLabel "item" AssetMatcher where
  fromLabel = AssetWithTrait Item

instance IsLabel "mystic" AssetMatcher where
  fromLabel = AssetWithClass Mystic

instance IsLabel "exhausted" AssetMatcher where
  fromLabel = AssetExhausted

instance Semigroup AssetMatcher where
  AssetWithHighestPrintedCost x <> AssetWithHighestPrintedCost y = AssetWithHighestPrintedCost (x <> y)
  AssetWithHighestPrintedCost x <> y = AssetWithHighestPrintedCost (x <> y)
  x <> AssetWithHighestPrintedCost y = AssetWithHighestPrintedCost (x <> y)
  AssetWithFewestClues x <> AssetWithFewestClues y = AssetWithFewestClues (x <> y)
  AssetWithFewestClues x <> y = AssetWithFewestClues (x <> y)
  x <> AssetWithFewestClues y = AssetWithFewestClues (x <> y)
  ClosestAsset lid x <> ClosestAsset lid' y
    | lid == lid' = AssetWithFewestClues (x <> y)
    | otherwise = error "Cannnot combine"
  ClosestAsset lid x <> y = ClosestAsset lid (x <> y)
  x <> ClosestAsset lid y = ClosestAsset lid (x <> y)
  AnyAsset <> x = x
  x <> AnyAsset = x
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]

instance Monoid AssetMatcher where
  mempty = AnyAsset

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | EnemyWithTrait Trait
  | EnemyWithToken Token
  | EnemyAt LocationMatcher
  | EnemyCanEnter LocationMatcher
  | EnemyCanMove
  | EnemyWithSealedChaosTokens Int ChaosTokenMatcher
  | EnemyWithoutTrait Trait
  | EnemyWithKeyword Keyword
  | EnemyWithClues ValueMatcher
  | EnemyWithEqualFields (Field Enemy Int) (Field Enemy Int)
  | EnemyWithNonZeroField (Field Enemy Int)
  | EnemyWithMaybeFieldLessThanOrEqualToThis EnemyId (Field Enemy (Maybe Int))
  | EnemyWithRemainingHealth ValueMatcher
  | EnemyWithDamage ValueMatcher
  | EnemyWithDoom ValueMatcher
  | EnemyWithMostDoom EnemyMatcher
  | EnemyIsEngagedWith InvestigatorMatcher
  | EnemyWithAsset AssetMatcher
  | EnemyWithAttachedEvent EventMatcher
  | NearestEnemy EnemyMatcher
  | FarthestEnemyFrom InvestigatorId EnemyMatcher
  | FarthestEnemyFromAll EnemyMatcher
  | NearestEnemyTo InvestigatorId EnemyMatcher
  | NearestEnemyToLocation LocationId EnemyMatcher
  | EnemyIs CardCode
  | EnemyWithCardId CardId
  | AnyEnemy
  | EnemyCanAttack InvestigatorMatcher
  | AttackingEnemy
  | AttackedYouSinceTheEndOfYourLastTurn
  | CanFightEnemy Source
  | CanEvadeEnemy Source -- This checks for an ability
  | EnemyCanBeEvadedBy Source -- This is not checking for an ability
  | EnemyCanBeDefeatedBy Source
  | CanFightEnemyWithOverride CriteriaOverride
  | CanEvadeEnemyWithOverride CriteriaOverride -- This checks for an ability but overrides the criteria
  | CanEngageEnemy Source
  | CanEngageEnemyWithOverride CriteriaOverride
  | EnemyDiscardedBy InvestigatorMatcher
  | ReadyEnemy
  | ExhaustedEnemy
  | NonWeaknessEnemy
  | EnemyInHandOf InvestigatorMatcher
  | CanParleyEnemy InvestigatorMatcher
  | EnemyMatchAll [EnemyMatcher]
  | EnemyOwnedBy InvestigatorMatcher
  | EnemyOneOf [EnemyMatcher]
  | EnemyWithMostRemainingHealth EnemyMatcher
  | EnemyWithoutModifier ModifierType
  | EnemyWithModifier ModifierType
  | EnemyWithEvade
  | EnemyWithFight
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  | EvadingEnemy
  | AttackedEnemy
  | IsIchtacasPrey
  | EnemyCanBeDamagedBySource Source
  | OutOfPlayEnemy OutOfPlayZone EnemyMatcher
  | IncludeOmnipotent EnemyMatcher
  | EnemyWithPlacement Placement
  | EnemyWithBounty -- Tony Morgan
  | PatrolEnemy
  | SwarmOf EnemyId
  | IsSwarm
  | IsHost
  | SwarmingEnemy
  | EnemyWithHealth
  | DefeatedEnemy EnemyMatcher
  | CanBeAttackedBy InvestigatorMatcher
  | EnemyWhenEvent EventMatcher
  | EnemyWhenLocation LocationMatcher
  | EnemyWhenInvestigator InvestigatorMatcher
  | EnemyWhenOtherEnemy EnemyMatcher
  | -- | Must be replaced
    ThatEnemy
  deriving stock (Show, Eq, Ord, Data)

instance Plated EnemyMatcher

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatchAll xs <> EnemyMatchAll ys = EnemyMatchAll (xs <> ys)
  EnemyMatchAll xs <> x = EnemyMatchAll (x : xs)
  x <> EnemyMatchAll xs = EnemyMatchAll (x : xs)
  x <> y = EnemyMatchAll [x, y]

instance Not EnemyMatcher where
  not_ = NotEnemy

instance Monoid EnemyMatcher where
  mempty = AnyEnemy

class IsEnemyMatcher a where
  toEnemyMatcher :: a -> EnemyMatcher

instance IsEnemyMatcher EnemyMatcher where
  toEnemyMatcher = id

instance IsEnemyMatcher EnemyId where
  toEnemyMatcher = EnemyWithId

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventWithCardId CardId
  | EventWithToken Token
  | EventControlledBy InvestigatorMatcher
  | EventOwnedBy InvestigatorMatcher
  | EventAt LocationMatcher
  | EventWithDoom ValueMatcher
  | EventAttachedToAsset AssetMatcher
  | EventAttachedTo TargetMatcher
  | EventWithModifier ModifierType
  | EventWithoutModifier ModifierType
  | EventIs CardCode
  | EventReady
  | EventCardMatch CardMatcher
  | EventMatches [EventMatcher]
  | EventOneOf [EventMatcher]
  | EnemyEvent EnemyId
  | AnyEvent
  | NotEvent EventMatcher
  | EventWithPlacement Placement
  | ActiveEvent
  | EventWithMetaKey Key
  deriving stock (Show, Eq, Ord, Data)

instance Not EventMatcher where
  not_ = NotEvent

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

type Where = LocationMatcher

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithUnrevealedTitle Text
  | LocationWithId LocationId
  | LocationWithLabel Label
  | LocationWithSymbol LocationSymbol
  | LocationLeavingPlay
  | LocationWithoutClues
  | LocationWithDoom ValueMatcher
  | LocationWithDamage ValueMatcher
  | LocationIs CardCode
  | LocationWithCardId CardId
  | Anywhere
  | Nowhere
  | HauntedLocation
  | EmptyLocation
  | LocationWithToken Token
  | ConnectedFrom LocationMatcher
  | ConnectedTo LocationMatcher
  | AccessibleFrom LocationMatcher
  | AccessibleTo LocationMatcher
  | LocationWithVictory
  | LocationWithDistanceFrom Int LocationMatcher
  | -- | distance, start, end
    LocationWithDistanceFromAtMost Int LocationMatcher LocationMatcher
  | LocationWithDistanceFromAtLeast Int LocationMatcher LocationMatcher
  | -- | distance, valid step, start, destination
    LocationWithAccessiblePath Source Int InvestigatorMatcher LocationMatcher
  | CanMoveCloserToLocation Source InvestigatorMatcher LocationMatcher
  | LocationBetween LocationMatcher LocationMatcher LocationMatcher
  | LocationWithResources ValueMatcher
  | LocationWithClues ValueMatcher
  | LocationWithHorror ValueMatcher
  | LocationWithShroud ValueMatcher
  | LocationWithShroudLessThanOrEqualToLessThanEnemyMaybeField EnemyId (Field Enemy (Maybe Int))
  | LocationWithMostClues LocationMatcher
  | LocationWithEnemy EnemyMatcher
  | LocationCanBeEnteredBy EnemyId
  | LocationWithAsset AssetMatcher
  | LocationWithInvestigator InvestigatorMatcher
  | CanEnterLocation InvestigatorMatcher
  | CanMoveToLocation InvestigatorMatcher Source LocationMatcher
  | RevealedLocation
  | UnrevealedLocation
  | InvestigatableLocation
  | LocationNotInPlay
  | LocationFartherFrom LocationId LocationMatcher
  | FarthestLocationFromLocation LocationId LocationMatcher
  | NearestLocationToLocation LocationId LocationMatcher
  | --                           ^ start
    FarthestLocationFromInvestigator InvestigatorMatcher LocationMatcher
  | FarthestLocationFromAll LocationMatcher
  | NearestLocationToYou LocationMatcher
  | NearestLocationTo InvestigatorId LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTrait Trait
  | LocationInDirection Direction LocationMatcher
  | LocationWithTreachery TreacheryMatcher
  | LocationWithoutTreachery TreacheryMatcher
  | LocationWithoutModifier ModifierType
  | LocationWithModifier ModifierType
  | LocationWithDiscoverableCluesBy InvestigatorMatcher
  | LocationMatchAll [LocationMatcher]
  | LocationMatchAny [LocationMatcher]
  | FirstLocation [LocationMatcher]
  | NotLocation LocationMatcher
  | LocationBeingDiscovered
  | LocationCanBeFlipped
  | SingleSidedLocation
  | ClosestPathLocation LocationId LocationId
  | LocationWithDefeatedEnemyThisRound
  | HighestShroud LocationMatcher
  | -- | start destination / end destination
    LocationWithLowerPrintedShroudThan LocationMatcher
  | BlockedLocation
  | -- | only useful for windows
    ThisLocation
  | -- | Scenario specific criteria
    LocationIsInFrontOf InvestigatorMatcher
  | IsIchtacasDestination
  | LocationWithBrazier Brazier
  | LocationWithBreaches ValueMatcher
  | LocationWithIncursion
  | FewestBreaches
  | MostBreaches LocationMatcher
  | IncludeEmptySpace LocationMatcher
  | LocationWhenCriteria Criterion
  | -- | Must be replaced
    ThatLocation
  deriving stock (Show, Eq, Ord, Data)

newtype LocationFilter = LocationFilter {getLocationFilter :: LocationMatcher}

-- LocationFilter has the same semigroup and monoid instances as LocationMatcher except that the monoid instance is Nowhere and the Semigroup instance needs to swap the behavior for Anywhere and Nowhere

instance Semigroup LocationFilter where
  LocationFilter Nowhere <> y = y
  x <> LocationFilter Nowhere = x
  x@(LocationFilter Anywhere) <> _ = x
  _ <> y@(LocationFilter Anywhere) = y
  LocationFilter (LocationMatchAll xs) <> LocationFilter (LocationMatchAll ys) =
    LocationFilter $ LocationMatchAll (xs <> ys)
  LocationFilter (LocationMatchAll xs) <> LocationFilter x = LocationFilter (LocationMatchAll (x : xs))
  LocationFilter x <> LocationFilter (LocationMatchAll xs) = LocationFilter (LocationMatchAll (x : xs))
  LocationFilter x <> LocationFilter y = LocationFilter $ LocationMatchAll [x, y]

instance Monoid LocationFilter where
  mempty = LocationFilter Nowhere

instance Plated LocationMatcher

instance Not LocationMatcher where
  not_ = NotLocation

class IsMatcher a
instance IsMatcher LocationMatcher
instance IsMatcher EnemyMatcher
instance IsMatcher AssetMatcher
instance IsMatcher EventMatcher
instance IsMatcher InvestigatorMatcher
instance IsMatcher TreacheryMatcher
class IsMatcher b => Be a b where
  be :: a -> b

instance Be InvestigatorMatcher InvestigatorMatcher where
  be = id

instance Be LocationMatcher LocationMatcher where
  be = id

instance Be LocationId LocationMatcher where
  be = LocationWithId

instance Be EnemyId EnemyMatcher where
  be = EnemyWithId

instance Be TreacheryId TreacheryMatcher where
  be = TreacheryWithId

instance IsString LocationMatcher where
  fromString = LocationWithTitle . fromString

class IsLocationMatcher a where
  toLocationMatcher :: a -> LocationMatcher

instance IsLocationMatcher LocationMatcher where
  toLocationMatcher = id

instance IsLocationMatcher LocationId where
  toLocationMatcher = LocationWithId

instance Semigroup LocationMatcher where
  Anywhere <> x = x
  x <> Anywhere = x
  Nowhere <> _ = Nowhere
  _ <> Nowhere = Nowhere
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll $ xs <> ys
  LocationMatchAll xs <> x = LocationMatchAll (x : xs)
  x <> LocationMatchAll xs = LocationMatchAll (x : xs)
  x <> y = LocationMatchAll [x, y]

instance Monoid LocationMatcher where
  mempty = Anywhere

newtype AnyLocationMatcher = AnyLocationMatcher {getAnyLocationMatcher :: LocationMatcher}

instance Semigroup AnyLocationMatcher where
  AnyLocationMatcher l <> AnyLocationMatcher r =
    AnyLocationMatcher $ case (l, r) of
      (Nowhere, x) -> x
      (x, Nowhere) -> x
      (LocationMatchAny xs, LocationMatchAny ys) -> LocationMatchAny $ xs <> ys
      (LocationMatchAny xs, x) -> LocationMatchAny (x : xs)
      (x, LocationMatchAny xs) -> LocationMatchAny (x : xs)
      (x, y) -> LocationMatchAny [x, y]

instance Monoid AnyLocationMatcher where
  mempty = AnyLocationMatcher Nowhere

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillWithCardId CardId
  | SkillControlledBy InvestigatorMatcher
  | SkillOwnedBy InvestigatorMatcher
  | SkillWithPlacement Placement
  | SkillMatches [SkillMatcher]
  | SkillIs CardCode
  | EligibleSkill
  | YourSkill
  | AnySkill
  | EnemySkill EnemyId
  | NotSkill SkillMatcher
  | SkillWithToken Token
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup SkillMatcher where
  AnySkill <> x = x
  x <> AnySkill = x
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]

instance Monoid SkillMatcher where
  mempty = AnySkill

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithToken Token
  | TreacheryWithDoom ValueMatcher
  | TreacheryWithHorror ValueMatcher
  | TreacheryWithTrait Trait
  | TreacheryInHandOf InvestigatorMatcher
  | TreacheryInThreatAreaOf InvestigatorMatcher
  | TreacheryIs CardCode
  | TreacheryIsAttachedTo Target
  | TreacheryAttachedToLocation LocationMatcher
  | TreacheryWithCardId CardId
  | TreacheryAt LocationMatcher
  | TreacheryOnEnemy EnemyMatcher
  | TreacheryIsNonWeakness
  | TreacheryWithResolvedEffectsBy InvestigatorMatcher
  | TreacheryDiscardedBy InvestigatorMatcher
  | TreacheryWithModifier ModifierType
  | AnyTreachery
  | InPlayTreachery
  | HiddenTreachery
  | TreacheryOwnedBy InvestigatorMatcher
  | TreacheryMatches [TreacheryMatcher]
  | TreacheryOneOf [TreacheryMatcher]
  | NotTreachery TreacheryMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not TreacheryMatcher where
  not_ = NotTreachery

instance Semigroup TreacheryMatcher where
  AnyTreachery <> x = x
  x <> AnyTreachery = x
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

instance Monoid TreacheryMatcher where
  mempty = AnyTreachery

-- | Relies on game state, can not be used purely
data ExtendedCardMatcher
  = BasicCardMatch CardMatcher
  | WillGoIntoSlot SlotType
  | CardIsBeneathInvestigator Who
  | CardIsBeneathAsset AssetMatcher
  | CardIsAsset AssetMatcher
  | CardWithCopyInHand Who
  | CardIsAttachedToLocation LocationMatcher
  | NotThisCard
  | IsThisCard
  | ControlledBy Who
  | OwnedBy Who
  | InHandOf Who
  | InDeckOf Who
  | InPlayAreaOf Who
  | InDiscardOf Who
  | TopOfDeckOf Who
  | EligibleForCurrentSkillTest
  | SetAsideCardMatch CardMatcher
  | UnderScenarioReferenceMatch CardMatcher
  | VictoryDisplayCardMatch CardMatcher
  | HandCardWithDifferentTitleFromAtLeastOneAsset InvestigatorMatcher AssetMatcher CardMatcher
  | ExtendedCardWithOneOf [ExtendedCardMatcher]
  | ExtendedCardMatches [ExtendedCardMatcher]
  | PlayableCardWithCostReduction ActionStatus Int ExtendedCardMatcher
  | PlayableCardWithNoCost ActionStatus ExtendedCardMatcher
  | PlayableCard CostStatus ExtendedCardMatcher
  | PlayableCardWithCriteria ActionStatus CriteriaOverride ExtendedCardMatcher
  | CommittableCard InvestigatorMatcher ExtendedCardMatcher
  | CardWithPerformableAbility AbilityMatcher [ModifierType]
  | CanCancelRevelationEffect ExtendedCardMatcher
  | CanCancelAllEffects ExtendedCardMatcher
  | CardWithoutModifier ModifierType
  | CardIsCommittedBy InvestigatorMatcher
  | ChosenViaCustomization ExtendedCardMatcher
  | PassesCommitRestrictions ExtendedCardMatcher
  | CardWithSharedTraitToAttackingEnemy
  deriving stock (Show, Eq, Ord, Data)

instance Plated ExtendedCardMatcher

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  ExtendedCardMatches xs <> x = ExtendedCardMatches (x : xs)
  x <> ExtendedCardMatches xs = ExtendedCardMatches (x : xs)
  x <> y = ExtendedCardMatches [x, y]

instance IsLabel "any" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #any

instance IsLabel "guardian" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #guardian

instance IsLabel "seeker" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #seeker

instance IsLabel "rogue" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #rogue

instance IsLabel "mystic" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #mystic

instance IsLabel "survivor" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #survivor

instance IsLabel "ally" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #ally

instance IsLabel "skill" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #skill

instance IsLabel "spell" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #spell

instance IsLabel "item" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #item

instance IsLabel "tome" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #tome

instance IsLabel "ritual" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #ritual

instance IsLabel "illicit" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #illicit

instance IsLabel "tool" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #tool

instance IsLabel "weapon" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #weapon

instance IsLabel "asset" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #asset

instance IsLabel "event" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #event

instance IsLabel "enemy" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #enemy

instance IsLabel "treachery" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #treachery

instance IsLabel "weakness" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #weakness

instance IsLabel "parley" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #parley

instance IsLabel "eligible" ExtendedCardMatcher where
  fromLabel = EligibleForCurrentSkillTest

-- | Only relies on card state, can be used purely with `cardMatch`
data CardMatcher
  = CardWithType CardType
  | CardWithSubType CardSubType
  | CardWithCardCode CardCode
  | CardWithTitle Text
  | CardWithTrait Trait
  | CardWithId CardId
  | CardWithLevel Int
  | CardWithMaxLevel Int
  | CardWithoutKeyword Keyword
  | CardWithKeyword Keyword
  | CardWithClass ClassSymbol
  | CardWithAction Action
  | CardWithoutAction
  | CardWithSkillIcon SkillIcon
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | CardWithPrintedLocationConnection LocationSymbol
  | CardWithPrintedLocationSymbol LocationSymbol
  | NotCard CardMatcher
  | IsEncounterCard
  | CardIsUnique
  | FastCard
  | NonWeakness
  | NonSignature
  | WeaknessCard
  | BasicWeaknessCard
  | NonExceptional
  | AnyCard
  | CardWithVengeance
  | CardFillsSlot SlotType
  | DiscardableCard
  | CardWithRevelation
  | CardOwnedBy InvestigatorId
  | CardFromEncounterSet EncounterSet
  | CardWithOddCost
  | CardWithEvenCost
  | CardWithCost Int
  | CardWithNonZeroCost
  | CardWithOddSkillIcons
  | CardWithEvenSkillIcons
  | CardWithAnySkills
  | CardWithNoSkills
  | CardWithOddNumberOfWordsInTitle
  | CardWithEvenNumberOfWordsInTitle
  | CardWithAvailableCustomization
  deriving stock (Show, Eq, Ord, Data)

instance Not CardMatcher where
  not_ = NotCard

instance IsString CardMatcher where
  fromString = CardWithTitle . fromString

instance IsLabel "any" CardMatcher where
  fromLabel = AnyCard

instance IsLabel "blessed" CardMatcher where
  fromLabel = CardWithTrait Blessed

instance IsLabel "cursed" CardMatcher where
  fromLabel = CardWithTrait Cursed

instance IsLabel "illicit" CardMatcher where
  fromLabel = CardWithTrait Illicit

instance IsLabel "tactic" CardMatcher where
  fromLabel = CardWithTrait Tactic

instance IsLabel "insight" CardMatcher where
  fromLabel = CardWithTrait Insight

instance IsLabel "tarot" CardMatcher where
  fromLabel = CardWithTrait Tarot

instance IsLabel "tome" CardMatcher where
  fromLabel = CardWithTrait Tome

instance IsLabel "spell" CardMatcher where
  fromLabel = CardWithTrait Spell

instance IsLabel "ritual" CardMatcher where
  fromLabel = CardWithTrait Ritual

instance IsLabel "item" CardMatcher where
  fromLabel = CardWithTrait Item

instance IsLabel "supply" CardMatcher where
  fromLabel = CardWithTrait Trait.Supply

instance IsLabel "tool" CardMatcher where
  fromLabel = CardWithTrait Tool

instance IsLabel "weapon" CardMatcher where
  fromLabel = CardWithTrait Weapon

instance IsLabel "guardian" CardMatcher where
  fromLabel = CardWithClass Guardian

instance IsLabel "mystic" CardMatcher where
  fromLabel = CardWithClass Mystic

instance IsLabel "survivor" CardMatcher where
  fromLabel = CardWithClass Survivor

instance IsLabel "seeker" CardMatcher where
  fromLabel = CardWithClass Seeker

instance IsLabel "rogue" CardMatcher where
  fromLabel = CardWithClass Rogue

instance IsLabel "location" CardMatcher where
  fromLabel = CardWithType LocationType

instance IsLabel "treachery" CardMatcher where
  fromLabel = CardWithType TreacheryType

instance IsLabel "event" CardMatcher where
  fromLabel = CardWithOneOf [CardWithType EventType, CardWithType EncounterEventType]

instance IsLabel "skill" CardMatcher where
  fromLabel = CardWithType SkillType

instance IsLabel "enemy" CardMatcher where
  fromLabel = CardWithType EnemyType

instance IsLabel "asset" CardMatcher where
  fromLabel = CardWithOneOf [CardWithType AssetType, CardWithType EncounterAssetType]

instance IsLabel "ally" CardMatcher where
  fromLabel = CardWithTrait Ally

instance IsLabel "parley" CardMatcher where
  fromLabel = CardWithAction #parley

instance IsLabel "weakness" CardMatcher where
  fromLabel = WeaknessCard

isEnemyCard :: CardMatcher -> CardMatcher
isEnemyCard = (#enemy <>)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches $ xs <> ys
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

class IsCardMatcher a where
  toCardMatcher :: a -> CardMatcher

instance IsCardMatcher CardMatcher where
  toCardMatcher = id
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher CardType where
  toCardMatcher = CardWithType
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher Trait where
  toCardMatcher = CardWithTrait
  {-# INLINE toCardMatcher #-}

data DiscardedPlayerCardMatcher
  = DiscardedCardMatcher InvestigatorMatcher CardMatcher
  deriving stock (Show, Eq, Ord, Data)

data MovesVia = MovedViaHunter | MovedViaOther | MovedViaAny
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

type FromWhere = Where
type ToWhere = Where

data WindowMatcher
  = EnemyDefeated Timing Who DefeatedByMatcher EnemyMatcher
  | FirstTimeParleyingThisRound Timing Who
  | SpentUses Timing Who SourceMatcher UseType AssetMatcher ValueMatcher
  | AttackOrEffectSpentLastUse Timing SourceMatcher TargetMatcher UseType
  | WouldPayCardCost Timing Who CardMatcher
  | WouldBeShuffledIntoDeck DeckMatcher CardMatcher
  | AddedToVictory Timing CardMatcher
  | PerformAction Timing Who ActionMatcher
  | PerformedSameTypeOfAction Timing Who ActionMatcher
  | DrawingStartingHand Timing Who
  | InvestigatorDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldBeDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldTakeDamage Timing Who SourceMatcher DamageTypeMatcher
  | InvestigatorWouldTakeHorror Timing Who SourceMatcher
  | WouldSearchDeck Timing Who DeckMatcher
  | WouldLookAtDeck Timing Who DeckMatcher
  | LookedAtDeck Timing Who DeckMatcher
  | SearchedDeck Timing Who DeckMatcher
  | AmongSearchedCards Who
  | DeckWouldRunOutOfCards Timing Who
  | DeckHasNoCards Timing Who
  | EncounterDeckRunsOutOfCards
  | MovedBy Timing Who SourceMatcher
  | MovedButBeforeEnemyEngagement Timing Who Where
  | MovedFromHunter Timing EnemyMatcher
  | ChosenRandomLocation Timing LocationMatcher
  | PlaceUnderneath Timing TargetMatcher CardMatcher
  | WouldPlaceDoomCounter Timing SourceMatcher TargetMatcher
  | PlacedDoomCounter Timing SourceMatcher TargetMatcher
  | PlacedDoomCounterOnTargetWithNoDoom Timing SourceMatcher TargetMatcher
  | SpentClues Timing InvestigatorMatcher ValueMatcher
  | PlacedToken Timing SourceMatcher TargetMatcher Token
  | InvestigatorPlacedFromTheirPool Timing Who SourceMatcher TargetMatcher Token
  | WouldPlaceBreach Timing TargetMatcher
  | PlacedBreach Timing TargetMatcher
  | PlacedBreaches Timing TargetMatcher
  | WouldRemoveBreach Timing TargetMatcher
  | RemovedBreach Timing TargetMatcher
  | RemovedBreaches Timing TargetMatcher
  | EnemyWouldBeDefeated Timing EnemyMatcher
  | EnemyWouldReady Timing EnemyMatcher
  | EnemyReadies Timing EnemyMatcher
  | EnemyEnters Timing Where EnemyMatcher
  | EnemyLeaves Timing Where EnemyMatcher
  | AgendaAdvances Timing AgendaMatcher
  | ActAdvances Timing ActMatcher
  | AgendaWouldAdvance Timing AgendaAdvancementReason AgendaMatcher
  | AssetDefeated Timing DefeatedByMatcher AssetMatcher
  | AttemptToEvade Timing Who EnemyMatcher
  | AttachCard Timing (Maybe Who) CardMatcher TargetMatcher
  | EnemyEvaded Timing Who EnemyMatcher
  | EnemyEngaged Timing Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | LocationEntersPlay Timing LocationMatcher
  | TreacheryEntersPlay Timing TreacheryMatcher
  | AgendaEntersPlay Timing AgendaMatcher
  | AssetEntersPlay Timing AssetMatcher
  | AssetLeavesPlay Timing AssetMatcher
  | AssetDealtDamage Timing SourceMatcher AssetMatcher
  | AssetDealtDamageOrHorror Timing SourceMatcher AssetMatcher
  | LastClueRemovedFromAsset Timing AssetMatcher
  | EnemyDealtDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyDealtExcessDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyTakeDamage Timing DamageEffectMatcher EnemyMatcher ValueMatcher SourceMatcher
  | InvestigatorTakeDamage Timing Who SourceMatcher
  | InvestigatorTakeHorror Timing Who SourceMatcher
  | EnemyLeavesPlay Timing EnemyMatcher
  | LocationLeavesPlay Timing LocationMatcher
  | TookControlOfAsset Timing Who AssetMatcher
  | DiscoveringLastClue Timing Who Where
  | DiscoverClues Timing Who Where ValueMatcher
  | GainsClues Timing Who ValueMatcher
  | GainsResources Timing Who SourceMatcher ValueMatcher
  | EnemyWouldAttack Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacks Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacksEvenIfCancelled Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacked Timing Who SourceMatcher EnemyMatcher
  | EnemyAttackedSuccessfully Timing Who SourceMatcher EnemyMatcher
  | RevealChaosToken Timing Who ChaosTokenMatcher
  | RevealChaosTokensDuringSkillTest Timing Who SkillTestMatcher ChaosTokenMatcher
  | TokensWouldBeRemovedFromChaosBag Timing ChaosTokenMatcher
  | ResolvesChaosToken Timing Who ChaosTokenMatcher
  | CancelChaosToken Timing Who ChaosTokenMatcher
  | IgnoreChaosToken Timing Who ChaosTokenMatcher
  | WouldRevealChaosToken Timing Who
  | Discarded Timing (Maybe Who) SourceMatcher ExtendedCardMatcher
  | AssetHealed Timing DamageType AssetMatcher SourceMatcher
  | InvestigatorHealed Timing DamageType InvestigatorMatcher SourceMatcher
  | AssetWouldBeDiscarded Timing AssetMatcher
  | EventWouldBeDiscarded Timing EventMatcher
  | EnemyWouldBeDiscarded Timing EnemyMatcher
  | TreacheryWouldBeDiscarded Timing TreacheryMatcher
  | EnemyDiscarded Timing SourceMatcher EnemyMatcher
  | TreacheryDiscarded Timing SourceMatcher TreacheryMatcher
  | WouldPerformRevelationSkillTest Timing Who
  | InitiatedSkillTest Timing Who SkillTypeMatcher SkillTestValueMatcher SkillTestMatcher
  | SkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SkillTestEnded Timing Who SkillTestMatcher
  | PlacedCounter Timing Who SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnLocation Timing Where SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnEnemy Timing EnemyMatcher SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAgenda Timing AgendaMatcher SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAsset Timing AssetMatcher SourceMatcher CounterMatcher ValueMatcher
  | WouldHaveSkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SuccessfullyInvestigatedWithNoClues Timing Who Where
  | EnemyAttemptsToSpawnAt Timing EnemyMatcher LocationMatcher
  | EnemyWouldSpawnAt EnemyMatcher LocationMatcher
  | EnemySpawns Timing Where EnemyMatcher
  | EnemyMovedTo Timing Where MovesVia EnemyMatcher
  | FastPlayerWindow
  | TurnBegins Timing Who
  | TurnWouldEnd Timing Who
  | TurnEnds Timing Who
  | RoundBegins Timing
  | RoundEnds Timing
  | DuringTurn Who
  | Enters Timing Who Where
  | Leaves Timing Who Where
  | Moves Timing Who SourceMatcher FromWhere ToWhere
  | MoveAction Timing Who FromWhere ToWhere
  | OrWindowMatcher [WindowMatcher]
  | DealtDamage Timing SourceMatcher Who
  | DealtHorror Timing SourceMatcher Who
  | AssignedHorror Timing Who TargetListMatcher
  | DealtDamageOrHorror Timing SourceMatcher Who
  | WouldDrawEncounterCard Timing Who PhaseMatcher
  | WouldDrawCard Timing Who DeckMatcher
  | DrawCard Timing Who ExtendedCardMatcher DeckMatcher
  | DrawsCards Timing Who ValueMatcher
  | PlayCard Timing Who ExtendedCardMatcher
  | PlayEventDiscarding Timing Who EventMatcher
  | PhaseBegins Timing PhaseMatcher
  | PhaseEnds Timing PhaseMatcher
  | PlayerHasPlayableCard CostStatus ExtendedCardMatcher
  | RevealLocation Timing Who Where
  | FlipLocation Timing Who Where
  | PutLocationIntoPlay Timing Who Where
  | GameBegins Timing
  | GameEnds Timing
  | InvestigatorEliminated Timing Who
  | InvestigatorResigned Timing Who
  | AnyWindow
  | NotAnyWindow
  | CommittedCards Timing Who CardListMatcher
  | CommittedCard Timing Who CardMatcher
  | ActivateAbility Timing Who AbilityMatcher
  | Explored Timing Who ExploreMatcher
  | AttemptExplore Timing Who
  | PhaseStep Timing PhaseStepMatcher
  | SkillTestStep Timing SkillTestStep
  | AddingToCurrentDepth
  | CancelledOrIgnoredCardOrGameEffect SourceMatcher
  | LostResources Timing Who SourceMatcher
  | LostActions Timing Who SourceMatcher
  | WouldTriggerChaosTokenRevealEffectOnCard Who CardMatcher [ChaosTokenFace]
  | Exhausts Timing Who TargetMatcher
  | EnemyExhausts Timing EnemyMatcher
  | EntersThreatArea Timing Who CardMatcher
  | ScenarioCountIncremented Timing ScenarioCountKey
  | WindowWhen Criterion WindowMatcher
  deriving stock (Show, Eq, Ord, Data, Generic)

data PhaseStepMatcher = EnemiesAttackStep | HuntersMoveStep
  deriving stock (Show, Eq, Ord, Data)

data ExploreMatcher = SuccessfulExplore LocationMatcher | FailedExplore CardMatcher
  deriving stock (Show, Eq, Ord, Data)

data DefeatedByMatcher
  = ByHorror
  | ByDamage
  | ByOther
  | ByAny
  | BySource SourceMatcher
  | ByAnyOf [DefeatedByMatcher]
  | DefeatedByMatches [DefeatedByMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance Monoid DefeatedByMatcher where
  mempty = ByAny

instance Semigroup DefeatedByMatcher where
  ByAny <> x = x
  x <> ByAny = x
  DefeatedByMatches xs <> DefeatedByMatches ys = DefeatedByMatches (xs <> ys)
  DefeatedByMatches xs <> y = DefeatedByMatches (xs <> [y])
  y <> DefeatedByMatches xs = DefeatedByMatches (y : xs)
  x <> y = DefeatedByMatches [x, y]

data SkillTestMatcher
  = WhileInvestigating LocationMatcher
  | WhileAttackingAnEnemy EnemyMatcher
  | WhileEvadingAnEnemy EnemyMatcher
  | WhileParleyingWithAnEnemy EnemyMatcher
  | WhileParleying
  | SkillTestWithAction ActionMatcher
  | SkillTestWithSkill SkillMatcher
  | SkillTestWithSkillType SkillType
  | AnySkillTest
  | SkillTestWasFailed
  | YourSkillTest SkillTestMatcher
  | SkillTestAtYourLocation
  | SkillTestAt LocationMatcher
  | SkillTestOfInvestigator InvestigatorMatcher
  | SkillTestOnTreachery TreacheryMatcher
  | SkillTestOnLocation LocationMatcher
  | SkillTestOnAsset AssetMatcher
  | UsingThis
  | SkillTestOnEncounterCard
  | SkillTestSourceMatches SourceMatcher
  | SkillTestMatches [SkillTestMatcher]
  | SkillTestOneOf [SkillTestMatcher]
  | NotSkillTest SkillTestMatcher
  | SkillTestFromRevelation
  | SkillTestWithRevealedChaosToken ChaosTokenMatcher
  | SkillTestWithRevealedChaosTokenCount Int ChaosTokenMatcher
  | SkillTestWithResolvedChaosTokenBy InvestigatorMatcher ChaosTokenMatcher
  | SkillTestOnCardWithTrait Trait
  | SkillTestOnCard CardMatcher
  | SkillTestWithDifficulty ValueMatcher
  | PerilousSkillTest
  | IfSkillTestMatcher SkillTestMatcher SkillTestMatcher SkillTestMatcher
  deriving stock (Show, Eq, Ord, Data, Generic)

instance IsLabel "investigating" SkillTestMatcher where
  fromLabel = WhileInvestigating Anywhere

instance IsLabel "investigation" SkillTestMatcher where
  fromLabel = WhileInvestigating Anywhere

instance IsLabel "parley" SkillTestMatcher where
  fromLabel = WhileParleying

instance IsLabel "parleying" SkillTestMatcher where
  fromLabel = WhileParleying

instance IsLabel "fighting" SkillTestMatcher where
  fromLabel = WhileAttackingAnEnemy AnyEnemy

instance IsLabel "evading" SkillTestMatcher where
  fromLabel = WhileEvadingAnEnemy AnyEnemy

instance IsLabel "any" SkillTestMatcher where
  fromLabel = AnySkillTest

instance IsLabel "failed" SkillTestMatcher where
  fromLabel = SkillTestWasFailed

data SourceMatcher
  = SourceWithTrait Trait
  | SourceIsEnemyAttack EnemyMatcher
  | SourceIsTreacheryEffect TreacheryMatcher
  | SourceIsAsset AssetMatcher
  | SourceIsEvent EventMatcher
  | EncounterCardSource
  | SourceMatchesAny [SourceMatcher]
  | SourceOwnedBy InvestigatorMatcher
  | SourceIsType CardType
  | SourceIsPlayerCard
  | SourceIsPlayerCardAbility
  | AnySource
  | SourceIsCancelable SourceMatcher
  | SourceMatches [SourceMatcher]
  | NotSource SourceMatcher
  | SourceIs Source
  | SourceWithCard CardMatcher
  | SourceIsCardEffect
  deriving stock (Show, Eq, Ord, Data)

pattern AnyCancellableSource :: SourceMatcher
pattern AnyCancellableSource <- SourceIsCancelable AnySource
  where
    AnyCancellableSource = SourceIsCancelable AnySource

instance IsLabel "investigator" SourceMatcher where
  fromLabel = SourceIsType InvestigatorType

instance IsLabel "any" SourceMatcher where
  fromLabel = AnySource

instance Semigroup SourceMatcher where
  AnySource <> x = x
  x <> AnySource = x
  SourceMatches xs <> SourceMatches ys = SourceMatches $ xs <> ys
  SourceMatches xs <> x = SourceMatches $ xs <> [x]
  x <> SourceMatches xs = SourceMatches $ x : xs
  x <> y = SourceMatches [x, y]

data TargetMatcher
  = TargetIs Target
  | TargetMatchesAny [TargetMatcher]
  | AnyTarget
  | TargetMatches [TargetMatcher]
  | LocationTargetMatches LocationMatcher
  | ActTargetMatches ActMatcher
  | AgendaTargetMatches AgendaMatcher
  | AssetTargetMatches AssetMatcher
  | EnemyTargetMatches EnemyMatcher
  | ScenarioCardTarget
  | TargetWithDoom
  | TargetAtLocation LocationMatcher
  | NotTarget TargetMatcher
  | TargetWithTrait Trait
  deriving stock (Show, Eq, Ord, Data)

instance Not TargetMatcher where
  not_ = NotTarget

instance Semigroup TargetMatcher where
  AnyTarget <> x = x
  x <> AnyTarget = x
  TargetMatches xs <> TargetMatches ys = TargetMatches $ xs <> ys
  TargetMatches xs <> x = TargetMatches $ xs <> [x]
  x <> TargetMatches xs = TargetMatches $ x : xs
  x <> y = TargetMatches [x, y]

data TargetListMatcher
  = HasTarget TargetMatcher
  | ExcludesTarget TargetMatcher
  | AnyTargetList
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup SkillTestMatcher where
  AnySkillTest <> x = x
  x <> AnySkillTest = x
  SkillTestMatches xs <> SkillTestMatches ys = SkillTestMatches $ xs <> ys
  SkillTestMatches xs <> x = SkillTestMatches $ xs <> [x]
  x <> SkillTestMatches xs = SkillTestMatches $ x : xs
  x <> y = SkillTestMatches [x, y]

data SkillTestResultMatcher
  = FailureResult ValueMatcher
  | SuccessResult ValueMatcher
  | AnyResult
  | ResultOneOf [SkillTestResultMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "success" SkillTestResultMatcher where
  fromLabel = SuccessResult AnyValue

instance IsLabel "failure" SkillTestResultMatcher where
  fromLabel = FailureResult AnyValue

instance IsLabel "any" SkillTestResultMatcher where
  fromLabel = AnyResult

data ValueMatcher
  = LessThan GameValue
  | GreaterThan GameValue
  | LessThanOrEqualTo GameValue
  | GreaterThanOrEqualTo GameValue
  | EqualTo GameValue
  | AnyValue
  deriving stock (Show, Eq, Ord, Data)

data SkillTestValueMatcher
  = SkillTestGameValue ValueMatcher
  | GreaterThanBaseValue
  | AnySkillTestValue
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" SkillTestValueMatcher where
  fromLabel = AnySkillTestValue

data ChaosTokenMatcher
  = WithNegativeModifier
  | ChaosTokenFaceIs ChaosTokenFace
  | ChaosTokenFaceIsNot ChaosTokenFace
  | ChaosTokenMatchesAny [ChaosTokenMatcher]
  | AnyChaosToken
  | IsSymbol
  | InTokenPool ChaosTokenMatcher
  | ChaosTokenMatches [ChaosTokenMatcher]
  | IncludeSealed ChaosTokenMatcher
  | IncludeTokenPool ChaosTokenMatcher
  | WouldReduceYourSkillValueToZero
  | IsInfestationToken ChaosTokenMatcher
  | NotChaosToken ChaosTokenMatcher
  | SealedOnAsset AssetMatcher ChaosTokenMatcher
  | SealedOnEnemy EnemyMatcher ChaosTokenMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not ChaosTokenMatcher where
  not_ = NotChaosToken

instance IsLabel "any" ChaosTokenMatcher where
  fromLabel = AnyChaosToken

instance IsLabel "skull" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Skull

instance IsLabel "cultist" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ChaosToken.Cultist

instance IsLabel "tablet" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Tablet

instance IsLabel "elderthing" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ElderThing

instance IsLabel "eldersign" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ElderSign

instance IsLabel "autofail" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs AutoFail

instance IsLabel "bless" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs BlessToken

instance IsLabel "curse" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs CurseToken

instance Semigroup ChaosTokenMatcher where
  AnyChaosToken <> x = x
  x <> AnyChaosToken = x
  ChaosTokenMatches xs <> ChaosTokenMatches ys = ChaosTokenMatches $ xs <> ys
  ChaosTokenMatches xs <> x = ChaosTokenMatches $ xs <> [x]
  x <> ChaosTokenMatches xs = ChaosTokenMatches $ x : xs
  x <> y = ChaosTokenMatches [x, y]

instance Monoid ChaosTokenMatcher where
  mempty = AnyChaosToken

data PhaseMatcher = AnyPhase | IsMythosPhase | IsEnemyPhase | IsInvestigationPhase | IsUpkeepPhase
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" PhaseMatcher where
  fromLabel = AnyPhase

instance IsLabel "mythos" PhaseMatcher where
  fromLabel = IsMythosPhase

instance IsLabel "upkeep" PhaseMatcher where
  fromLabel = IsUpkeepPhase

instance IsLabel "enemy" PhaseMatcher where
  fromLabel = IsEnemyPhase

instance IsLabel "investigation" PhaseMatcher where
  fromLabel = IsInvestigationPhase

data WindowMythosStepMatcher
  = WhenAllDrawEncounterCard
  | AfterCheckDoomThreshold
  deriving stock (Show, Eq, Ord, Data)

data CounterMatcher = HorrorCounter | DamageCounter | ClueCounter | DoomCounter | ResourceCounter
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "horror" CounterMatcher where
  fromLabel = HorrorCounter

instance IsLabel "damage" CounterMatcher where
  fromLabel = DamageCounter

instance IsLabel "clue" CounterMatcher where
  fromLabel = ClueCounter

data ActionMatcher
  = ActionIs Action
  | AnyAction
  | ActionOneOf [ActionMatcher]
  | ActionMatches [ActionMatcher]
  | RepeatableAction
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup ActionMatcher where
  AnyAction <> x = x
  x <> AnyAction = x
  ActionMatches xs <> ActionMatches ys = ActionMatches $ xs <> ys
  ActionMatches xs <> x = ActionMatches $ xs <> [x]
  x <> ActionMatches xs = ActionMatches $ x : xs
  x <> y = ActionMatches [x, y]

instance Monoid ActionMatcher where
  mempty = AnyAction

instance IsLabel "activate" ActionMatcher where
  fromLabel = ActionIs #activate

instance IsLabel "engage" ActionMatcher where
  fromLabel = ActionIs #engage

instance IsLabel "evade" ActionMatcher where
  fromLabel = ActionIs #evade

instance IsLabel "fight" ActionMatcher where
  fromLabel = ActionIs #fight

instance IsLabel "investigate" ActionMatcher where
  fromLabel = ActionIs #investigate

instance IsLabel "move" ActionMatcher where
  fromLabel = ActionIs #move

instance IsLabel "parley" ActionMatcher where
  fromLabel = ActionIs #parley

instance IsLabel "play" ActionMatcher where
  fromLabel = ActionIs #play

instance IsLabel "resource" ActionMatcher where
  fromLabel = ActionIs #resource

instance IsLabel "draw" ActionMatcher where
  fromLabel = ActionIs #draw

data AbilityMatcher
  = AbilityOnLocation LocationMatcher
  | AbilityOnAsset AssetMatcher
  | AbilityOnEnemy EnemyMatcher
  | AbilityOnStory StoryMatcher
  | AbilityWindow WindowMatcher
  | AbilityIsAction Action
  | AbilityIsActionAbility
  | AbilityIsReactionAbility
  | AbilityIsFastAbility
  | AbilityIsForcedAbility
  | AbilityMatches [AbilityMatcher]
  | AbilityOneOf [AbilityMatcher]
  | AbilityIs Source Int
  | AnyAbility
  | BasicAbility
  | ActiveAbility
  | AbilityIsSkillTest
  | AbilityOnEncounterCard
  | AbilityOnCard CardMatcher
  | AbilityOnCardControlledBy InvestigatorId
  | AssetAbility AssetMatcher
  | HauntedAbility
  | PerformableAbility [ModifierType]
  | PerformableAbilityBy InvestigatorMatcher [ModifierType]
  | TriggeredAbility
  | NotAbility AbilityMatcher
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "parley" AbilityMatcher where
  fromLabel = AbilityIsAction #parley

instance IsLabel "draw" AbilityMatcher where
  fromLabel = AbilityIsAction #draw

instance IsLabel "play" AbilityMatcher where
  fromLabel = AbilityIsAction #play

instance IsLabel "engage" AbilityMatcher where
  fromLabel = AbilityIsAction #engage

instance IsLabel "resource" AbilityMatcher where
  fromLabel = AbilityIsAction #resource

instance IsLabel "move" AbilityMatcher where
  fromLabel = AbilityIsAction #move

instance IsLabel "resign" AbilityMatcher where
  fromLabel = AbilityIsAction #resign

instance IsLabel "investigate" AbilityMatcher where
  fromLabel = AbilityIsAction #investigate

instance IsLabel "evade" AbilityMatcher where
  fromLabel = AbilityIsAction #evade

instance IsLabel "action" AbilityMatcher where
  fromLabel = AbilityIsActionAbility

instance Not AbilityMatcher where
  not_ = NotAbility

instance Semigroup AbilityMatcher where
  AnyAbility <> x = x
  x <> AnyAbility = x
  AbilityMatches xs <> AbilityMatches ys = AbilityMatches $ xs <> ys
  AbilityMatches xs <> x = AbilityMatches $ xs <> [x]
  x <> AbilityMatches xs = AbilityMatches $ x : xs
  x <> y = AbilityMatches [x, y]

instance Monoid AbilityMatcher where
  mempty = AnyAbility

data CardListMatcher
  = LengthIs ValueMatcher
  | HasCard CardMatcher
  | AnyCards
  | DifferentLengthIsAtLeast Int CardMatcher
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "item" CardListMatcher where
  fromLabel = HasCard #item

data DeckMatcher
  = EncounterDeck
  | DeckOf InvestigatorMatcher
  | AnyDeck
  | DeckIs DeckSignifier
  | DeckOneOf [DeckMatcher]
  deriving stock (Show, Eq, Ord, Data)

data AgendaMatcher
  = AgendaWithId AgendaId
  | AgendaWithDoom ValueMatcher
  | AnyAgenda
  | AgendaWithTreachery TreacheryMatcher
  | AgendaWithSequence AgendaSequence
  | AgendaWithSide AgendaSide
  | AgendaWithDeckId Int
  | AgendaWithModifier ModifierType
  | NotAgenda AgendaMatcher
  | AgendaMatches [AgendaMatcher]
  | AgendaCanWheelOfFortuneX
  | FinalAgenda
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup AgendaMatcher where
  AnyAgenda <> x = x
  x <> AnyAgenda = x
  AgendaMatches xs <> AgendaMatches ys = AgendaMatches (xs <> ys)
  AgendaMatches xs <> x = AgendaMatches (x : xs)
  x <> AgendaMatches xs = AgendaMatches (x : xs)
  x <> y = AgendaMatches [x, y]

data ActMatcher
  = ActWithId ActId
  | AnyAct
  | ActWithSide ActSide
  | ActWithTreachery TreacheryMatcher
  | ActWithDeckId Int
  | NotAct ActMatcher
  | ActOneOf [ActMatcher]
  | ActCanWheelOfFortuneX
  deriving stock (Show, Eq, Ord, Data)

data DamageEffectMatcher
  = AttackDamageEffect
  | NonAttackDamageEffect
  | AnyDamageEffect
  deriving stock (Show, Eq, Ord, Data)

data EnemyAttackMatcher
  = AnyEnemyAttack
  | AttackOfOpportunityAttack
  | AttackOfOpportunityAttackYouProvoked
  | AttackViaAlert
  | CancelableEnemyAttack EnemyAttackMatcher
  | NotEnemyAttack EnemyAttackMatcher
  | AttackDamagedAsset AssetMatcher
  | AttackDealtDamageOrHorror
  | EnemyAttackMatches [EnemyAttackMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup EnemyAttackMatcher where
  AnyEnemyAttack <> x = x
  x <> AnyEnemyAttack = x
  EnemyAttackMatches xs <> EnemyAttackMatches ys = EnemyAttackMatches $ xs <> ys
  EnemyAttackMatches xs <> x = EnemyAttackMatches $ xs <> [x]
  x <> EnemyAttackMatches xs = EnemyAttackMatches $ x : xs
  x <> y = EnemyAttackMatches [x, y]

instance Monoid EnemyAttackMatcher where
  mempty = AnyEnemyAttack

instance Not EnemyAttackMatcher where
  not_ = NotEnemyAttack

data ScenarioMatcher = TheScenario
  deriving stock (Show, Eq, Ord, Data)

data CampaignMatcher = TheCampaign
  deriving stock (Show, Eq, Ord, Data)

data EffectMatcher
  = AnyEffect
  | EffectWithCardCode CardCode
  | EffectWithMetaInt Int
  | EffectWithTarget Target
  | EffectMatches [EffectMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup EffectMatcher where
  AnyEffect <> x = x
  x <> AnyEffect = x
  EffectMatches xs <> EffectMatches ys = EffectMatches $ xs <> ys
  EffectMatches xs <> x = EffectMatches $ xs <> [x]
  x <> EffectMatches xs = EffectMatches $ x : xs
  x <> y = EffectMatches [x, y]

instance Monoid EffectMatcher where
  mempty = AnyEffect

data SkillTypeMatcher
  = AnySkillType
  | NotSkillType SkillType
  | IsSkillType SkillType
  | SkillTypeOneOf [SkillType]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" SkillTypeMatcher where
  fromLabel = AnySkillType

data RemoveDoomMatchers = RemoveDoomMatchers
  { removeDoomLocations :: LocationMatcher
  , removeDoomInvestigators :: InvestigatorMatcher
  , removeDoomEnemies :: EnemyMatcher
  , removeDoomAssets :: AssetMatcher
  , removeDoomActs :: ActMatcher
  , removeDoomAgendas :: AgendaMatcher
  , removeDoomTreacheries :: TreacheryMatcher
  , removeDoomEvents :: EventMatcher
  , removeDoomSkills :: SkillMatcher
  }
  deriving stock (Show, Eq, Ord, Data)

newtype RemainingActMatcher = RemainingActMatcher {unRemainingActMatcher :: ActMatcher}
  deriving stock (Show, Eq, Ord)

data StoryMatcher
  = StoryWithTitle Text
  | StoryWithPlacement Placement
  | StoryMatchAll [StoryMatcher]
  | StoryIs CardCode
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup StoryMatcher where
  StoryMatchAll xs <> StoryMatchAll ys = StoryMatchAll (xs <> ys)
  StoryMatchAll xs <> x = StoryMatchAll $ xs <> [x]
  x <> StoryMatchAll xs = StoryMatchAll (x : xs)
  x <> y = StoryMatchAll [x, y]

data AspectMatcher = AspectIs Aspect
  deriving stock (Show, Eq, Ord, Data)

data HistoryMatcher = DefeatedEnemiesWithTotalHealth ValueMatcher
  deriving stock (Show, Eq, Ord, Data)

data DamageTypeMatcher = IsDirectDamage | IsNonDirectDamage | AnyDamageType
  deriving stock (Show, Eq, Ord, Data)

$( do
    ability <- deriveJSON defaultOptions ''AbilityMatcher
    act <- deriveJSON defaultOptions ''ActMatcher
    action <- deriveJSON defaultOptions ''ActionMatcher
    agenda <- deriveJSON defaultOptions ''AgendaMatcher
    aspect <- deriveJSON defaultOptions ''AspectMatcher
    asset <- deriveJSON defaultOptions ''AssetMatcher
    card <- deriveJSON defaultOptions ''CardMatcher
    cardList <- deriveJSON defaultOptions ''CardListMatcher
    chaosToken <- deriveJSON defaultOptions ''ChaosTokenMatcher
    counter <- deriveJSON defaultOptions ''CounterMatcher
    damageEffect <- deriveJSON defaultOptions ''DamageEffectMatcher
    damageType <- deriveJSON defaultOptions ''DamageTypeMatcher
    deck <- deriveJSON defaultOptions ''DeckMatcher
    defeatedBy <- deriveJSON defaultOptions ''DefeatedByMatcher
    enemy <- deriveJSON defaultOptions ''EnemyMatcher
    enemyAttack <- deriveJSON defaultOptions ''EnemyAttackMatcher
    event <- deriveJSON defaultOptions ''EventMatcher
    explore <- deriveJSON defaultOptions ''ExploreMatcher
    extendedCard <- deriveJSON defaultOptions ''ExtendedCardMatcher
    history <- deriveJSON defaultOptions ''HistoryMatcher
    investigator <- deriveJSON defaultOptions ''InvestigatorMatcher
    location <- deriveJSON defaultOptions ''LocationMatcher
    phase <- deriveJSON defaultOptions ''PhaseMatcher
    phaseStep <- deriveJSON defaultOptions ''PhaseStepMatcher
    prey <- deriveJSON defaultOptions ''PreyMatcher
    removeDoom <- deriveJSON defaultOptions ''RemoveDoomMatchers
    skill <- deriveJSON defaultOptions ''SkillMatcher
    skillTest <- deriveToJSON defaultOptions ''SkillTestMatcher
    skillTestResult <- deriveJSON defaultOptions ''SkillTestResultMatcher
    skillTestValue <- deriveJSON defaultOptions ''SkillTestValueMatcher
    skillType <- deriveJSON defaultOptions ''SkillTypeMatcher
    source <- deriveJSON defaultOptions ''SourceMatcher
    story <- deriveJSON defaultOptions ''StoryMatcher
    target <- deriveJSON defaultOptions ''TargetMatcher
    targetList <- deriveJSON defaultOptions ''TargetListMatcher
    treachery <- deriveJSON defaultOptions ''TreacheryMatcher
    value <- deriveJSON defaultOptions ''ValueMatcher
    window <- deriveToJSON defaultOptions ''WindowMatcher
    windowMythosStep <- deriveJSON defaultOptions ''WindowMythosStepMatcher
    pure
      $ concat
        [ ability
        , act
        , action
        , agenda
        , aspect
        , asset
        , card
        , cardList
        , chaosToken
        , counter
        , damageEffect
        , damageType
        , deck
        , defeatedBy
        , enemy
        , enemyAttack
        , event
        , explore
        , extendedCard
        , history
        , investigator
        , location
        , phase
        , phaseStep
        , prey
        , removeDoom
        , skill
        , skillTest
        , skillTestResult
        , skillTestValue
        , skillType
        , source
        , story
        , target
        , targetList
        , treachery
        , value
        , window
        , windowMythosStep
        ]
 )

instance FromJSON SkillTestMatcher where
  parseJSON = withObject "SkillTestMatcher" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "AnySkillTestType" -> pure AnySkillTest
      _ -> genericParseJSON defaultOptions (Object o)

instance FromJSON WindowMatcher where
  parseJSON = withObject "WindowMatcher" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "EnemyAttackedSuccessfully" -> do
        econtents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case econtents of
          Left (a, b, c) -> pure $ EnemyAttackedSuccessfully a b AnySource c
          Right (a, b, c, d) -> pure $ EnemyAttackedSuccessfully a b c d
      _ -> genericParseJSON defaultOptions (Object o)
