module Arkham.Asset.Assets.WishEater (
  wishEater,
  WishEater (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Window qualified as Window

newtype WishEater = WishEater AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wishEater :: AssetCard WishEater
wishEater = asset WishEater Cards.wishEater

instance HasAbilities WishEater where
  getAbilities (WishEater attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #when You $ oneOf [#skull, #cultist, #tablet, #elderthing])
          (assetUseCost attrs Charge 1)
    , restrictedAbility
        attrs
        2
        ( ControlsThis <> exists (AssetWithId (toId attrs) <> AssetWithUseCount Charge (EqualTo $ Static 0))
        )
        $ ForcedAbility AnyWindow
    ]

instance RunMessage WishEater where
  runMessage msg a@(WishEater attrs) = case msg of
    CardIsEnteringPlay iid card | toCardId attrs == toCardId card -> do
      emptyVessel <- selectJust $ assetIs Cards.emptyVessel4
      charges <- fieldMap AssetUses (findWithDefault 0 Charge) emptyVessel
      attrs' <- runMessage msg attrs
      emptyVesselCard <- field AssetCard emptyVessel
      push $ PlaceInBonded iid emptyVesselCard
      pure
        $ WishEater
          (attrs' {assetTokens = singletonMap Charge charges <> assetTokens attrs'})
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      let source = toAbilitySource attrs 1
      healDamage <- canHaveDamageHealed source iid
      healHorror <- canHaveHorrorHealed source iid
      pushAll
        $ [HealDamage (InvestigatorTarget iid) source 1 | healDamage]
        <> [HealHorror (InvestigatorTarget iid) source 1 | healHorror]
      cancelChaosToken token
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      mEmptyVessel <- listToMaybe <$> searchBonded iid Cards.emptyVessel4
      for_ mEmptyVessel $ \emptyVessel -> do
        push $ PutCardIntoPlay iid emptyVessel Nothing NoPayment []
      push $ PlaceInBonded iid (toCard attrs)
      pure a
    _ -> WishEater <$> runMessage msg attrs
