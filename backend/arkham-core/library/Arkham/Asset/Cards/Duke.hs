module Arkham.Asset.Cards.Duke (Duke (..), duke) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection

newtype Duke = Duke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) (slotsL .~ mempty)

instance HasModifiersFor Duke where
  getModifiersFor (InvestigatorTarget iid) (Duke a) | controlledBy a iid = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource) of
      (Just Action.Fight, Just source) | isSource a source -> do
        pure $ toModifiers a [BaseSkillOf #combat 4, DamageDealt 1]
      (Just Action.Investigate, Just source) | isSource a source -> do
        pure $ toModifiers a [BaseSkillOf #intellect 4]
      _ -> pure []
  getModifiersFor _ _ = pure []

-- TODO: Duke's second ability doesn't bold Move so it is not a move action. I
-- don't remember the specific logic around ActionAbilityWithBefore but this
-- likely should change to just be an Investigate action
instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , restrictedAbility a 2 ControlsThis
        $ ActionAbilityWithBefore [#investigate] #move (ActionCost 1 <> exhaust a)
    ]

instance RunMessage Duke where
  runMessage msg a@(Duke attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkChooseFight sid iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 windows' _ -> do
      let source = attrs.ability 2
      lid <- getJustLocation iid
      accessibleLocationIds <- getAccessibleLocations iid source
      investigateAbilities <-
        filterM
          ( andM
              . sequence
                [ pure . (`abilityIs` #investigate)
                , (\ab -> getCanAffordAbility iid ab windows')
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                ]
          )
          =<< field LocationAbilities lid
      let
        investigateActions =
          map
            ( (\f -> f windows' [] [])
                . AbilityLabel iid
                . ( \a' ->
                      a'
                        { abilityDoesNotProvokeAttacksOfOpportunity = True
                        , abilitySource = ProxySource (abilitySource a') source
                        }
                  )
                . (`applyAbilityModifiers` [ActionCostModifier (-1)])
            )
            investigateAbilities
      player <- getPlayer iid
      push
        $ chooseOne player
        $ investigateActions
        <> [ targetLabel lid' [Move $ move attrs iid lid', DoStep 1 msg]
           | lid' <- accessibleLocationIds
           ]
      pure a
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      lid <- getJustLocation iid
      sid <- getRandom
      investigate' <- mkInvestigateLocation sid iid attrs lid
      push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigate']
      pure a
    UseCardAbility iid (ProxySource (LocationSource lid) (isAbilitySource attrs 2 -> True)) 101 _ _ -> do
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid attrs lid
      pure a
    _ -> Duke <$> runMessage msg attrs
