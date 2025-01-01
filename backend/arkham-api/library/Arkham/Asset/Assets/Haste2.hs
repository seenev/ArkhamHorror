module Arkham.Asset.Assets.Haste2 (haste2, Haste2 (..)) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

newtype Haste2 = Haste2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haste2 :: AssetCard Haste2
haste2 = asset Haste2 Cards.haste2

-- PerformedSameTypeOfAction
instance HasAbilities Haste2 where
  getAbilities (Haste2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( PerformedSameTypeOfAction
              #after
              You
              $ RepeatableAction
              <> oneOf [#activate, #engage, #evade, #fight, #investigate, #move, #play, #resource, #draw]
          )
          (exhaust a)
    ]

getActionTypes :: [Window] -> [Action]
getActionTypes [] = []
getActionTypes ((windowType -> Window.PerformedSameTypeOfAction _ as) : ws) =
  as <> getActionTypes ws
getActionTypes (_ : ws) = getActionTypes ws

instance RunMessage Haste2 where
  runMessage msg a@(Haste2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getActionTypes -> as) _ -> do
      a' <- getAttrs @Investigator iid
      actions <- withModifiers iid (toModifiers attrs [ActionCostModifier (-1)]) $ do
        getActions iid (defaultWindows iid)

      playableCards <-
        filter (`cardMatch` NotCard FastCard)
          <$> getPlayableCards a' (UnpaidCost NoAction) (defaultWindows iid)

      canAffordTakeResources <- withModifiers iid (toModifiers attrs [ActionCostOf IsAnyAction (-1)]) $ do
        getCanAfford a' [#resource]

      canAffordDrawCards <- withModifiers iid (toModifiers attrs [ActionCostOf IsAnyAction (-1)]) $ do
        getCanAfford a' [#draw]
      let available = filter (any (`elem` as) . abilityActions) actions
      player <- getPlayer iid
      let drawing = drawCards iid a' 1

      canDraw <- canDo iid #draw
      canTakeResource <- canDo iid #resource
      canPlay <- canDo iid #play
      push
        $ chooseOne player
        $ map (\ab -> AbilityLabel iid (decreaseAbilityActionCost ab 1) (defaultWindows iid) [] []) available
        <> [ ComponentLabel (InvestigatorComponent iid ResourceToken) [TakeResources iid 1 (toSource a') False]
           | canAffordTakeResources
           , canTakeResource
           , #resource `elem` as
           ]
        <> [ ComponentLabel (InvestigatorDeckComponent iid) [drawing]
           | canAffordDrawCards
           , canDraw
           , #draw `elem` as
           ]
        <> [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing NoPayment [] False]
           | canPlay
           , #play `elem` as
           , c <- playableCards
           ]
      pure a
    _ -> Haste2 <$> runMessage msg attrs
