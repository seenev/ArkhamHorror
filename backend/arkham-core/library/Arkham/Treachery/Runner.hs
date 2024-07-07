{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery.Runner (
  module X,
  addHiddenToHand,
  forcedOnElimination,
  on,
) where

import Arkham.Prelude

import Arkham.Calculation as X
import Arkham.Classes.Entity as X
import Arkham.Classes.HasAbilities as X
import Arkham.Classes.HasModifiersFor as X
import Arkham.Classes.Query as X
import Arkham.Classes.RunMessage as X
import Arkham.Helpers.Investigator as X (eliminationWindow)
import Arkham.Helpers.Message as X hiding (
  AssetDamage,
  DeckHasNoCards,
  EnemyDefeated,
  InvestigatorDamage,
  InvestigatorEliminated,
  RevealChaosToken,
  is,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Placement as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Helpers as X
import Arkham.Treachery.Types as X

import Arkham.Ability.Type
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Matcher (Be (..), TreacheryMatcher (TreacheryWithId))
import Arkham.Message qualified as Msg
import Arkham.Token
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

addHiddenToHand :: InvestigatorId -> TreacheryAttrs -> Message
addHiddenToHand iid a = PlaceTreachery (toId a) (HiddenInHand iid)

forcedOnElimination :: InvestigatorId -> AbilityType
forcedOnElimination = ForcedAbility . eliminationWindow

on :: Targetable target => TreacheryAttrs -> target -> Bool
on = treacheryOn

instance Be TreacheryAttrs TreacheryMatcher where
  be = TreacheryWithId . toId

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    Msg.SealedChaosToken token card | toCardId card == toCardId a -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    Msg.UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    Msg.RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    Msg.InvestigatorEliminated iid | InvestigatorTarget iid `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    Msg.InvestigatorEliminated iid | Just iid == treacheryOwner -> do
      push $ toDiscard GameSource a
      pure a
    PlaceTreachery tid placement | tid == treacheryId -> do
      for_ placement.attachedTo \target ->
        pushM $ checkAfter $ Window.AttachCard Nothing (toCard a) target
      let entersPlay = isOutOfPlayPlacement a.placement && isInPlayPlacement placement
      case placement of
        InThreatArea iid -> do
          pushM $ checkWindows $ Window.mkAfter (Window.EntersThreatArea iid $ toCard a)
            : [Window.mkAfter $ Window.TreacheryEntersPlay tid | entersPlay]
        _ -> when entersPlay do
          pushM $ checkAfter $ Window.TreacheryEntersPlay tid
      pure $ a & placementL .~ placement
    PlaceTokens _ (isTarget a -> True) token n -> pure $ a & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) token n -> pure $ a & tokensL %~ subtractTokens token n
    MoveTokens s source _ tType n | isSource a source -> runMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens s _ target tType n | isTarget a target -> runMessage (PlaceTokens s (toTarget a) tType n) a
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    Discard miid _ (TreacheryTarget tid) | tid == treacheryId -> do
      pure $ a & discardedByL .~ miid
    Discarded target _ _ | target `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    After (Revelation iid (isSource a -> True)) -> do
      pushWhen
        (treacheryPlacement == Limbo)
        (toDiscardBy iid iid a)
      pure $ a & resolvedL %~ insertSet iid
    RemoveAllAttachments source target -> do
      case a.placement.attachedTo of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    RemoveAllCopiesOfCardFromGame _ cCode | cCode == toCardCode a -> do
      push $ RemoveTreachery (toId a)
      pure a
    RemoveAllCopiesOfEncounterCardFromGame cardMatcher | toCard a `cardMatch` cardMatcher -> do
      push $ RemoveTreachery (toId a)
      pure a
    ReplaceAct aid _ -> do
      pushWhen (treacheryOnAct aid a) $ toDiscard (toSource a) (toTarget a)
      pure a
    ReplaceAgenda aid _ -> do
      pushWhen (treacheryOnAgenda aid a) $ toDiscard (toSource a) (toTarget a)
      pure a
    Discarded (isTarget a -> True) _ _ -> do
      runMessage (RemoveFromPlay (toSource a)) a
    RemoveFromPlay source | isSource a source -> do
      windowMsg <-
        checkWindows
          ( (`mkWindow` Window.LeavePlay (toTarget a))
              <$> [#when, #at, #after]
          )
      pushAll
        $ windowMsg
        : [UnsealChaosToken token | token <- treacherySealedChaosTokens]
          <> [RemovedFromPlay source]
      pure a
    _ -> pure a
