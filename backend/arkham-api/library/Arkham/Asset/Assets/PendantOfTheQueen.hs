module Arkham.Asset.Assets.PendantOfTheQueen (pendantOfTheQueen, PendantOfTheQueen (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Prelude
import Arkham.Taboo

newtype PendantOfTheQueen = PendantOfTheQueen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendantOfTheQueen :: AssetCard PendantOfTheQueen
pendantOfTheQueen = assetWith PendantOfTheQueen Cards.pendantOfTheQueen $ whenNoUsesL ?~ NotifySelfOfNoUses

{- Exhaust Pendant of the Queen and spend 1 charge: Choose a revealed location and select one - move to that location, discover 1 clue at that location, or automatically evade an enemy at that location. -}

instance HasAbilities PendantOfTheQueen where
  getAbilities (PendantOfTheQueen attrs) =
    [ controlledAbility
        attrs
        1
        ( oneOf
            [ youExist InvestigatorCanMove <> exists (RevealedLocation <> Unblocked <> NotYourLocation)
            , exists
                ( RevealedLocation
                    <> oneOf
                      [ LocationWithDiscoverableCluesBy You
                      , LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                      ]
                )
            ]
        )
        $ FastAbility (exhaust attrs <> assetUseCost attrs Charge 1)
    ]

instance RunMessage PendantOfTheQueen where
  runMessage msg a@(PendantOfTheQueen attrs) = case msg of
    SpentAllUses (isTarget attrs -> True) -> do
      if tabooed TabooList19 attrs
        then push $ RemoveFromGame (toTarget attrs)
        else for_ attrs.controller $ \controller -> do
          segments <- take 3 <$> searchBonded controller Cards.segmentOfOnyx1
          pushAll
            [ PlaceInBonded controller (toCard attrs)
            , ShuffleCardsIntoDeck (Deck.InvestigatorDeck controller) segments
            ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canMove <- iid <=~> InvestigatorCanMove
      locations <-
        select
          $ RevealedLocation
          <> oneOf
            ( LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
                : LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                : [not_ (locationWithInvestigator iid) <> Unblocked | canMove]
            )
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels locations (only . HandleTargetChoice iid (toAbilitySource attrs 1) . toTarget)

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      canMove <- iid <=~> InvestigatorCanMove
      moveChoice <- lid <=~> (NotLocation (locationWithInvestigator iid) <> Unblocked)
      discoverChoice <- lid <=~> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      enemies <-
        select $ EnemyAt (LocationWithId lid) <> EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded
      player <- getPlayer iid

      push
        $ chooseOrRunOne player
        $ [ Label "Move to this location" [Move $ move (attrs.ability 1) iid lid]
          | canMove && moveChoice
          ]
        <> [ Label
            "Discover a clue at this location"
            [Msg.DiscoverClues iid $ discover lid (attrs.ability 1) 1]
           | discoverChoice
           ]
        <> [ Label
            "Evade an enemy at this location"
            [chooseOrRunOne player $ targetLabels enemies (only . EnemyEvaded iid)]
           | notNull enemies
           ]
      pure a
    _ -> PendantOfTheQueen <$> runMessage msg attrs
