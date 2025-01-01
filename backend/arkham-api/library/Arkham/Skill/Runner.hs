{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Skill.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorEliminated,
  PlayCard,
  RevealLocation,
 )
import Arkham.Helpers.Query as X
import Arkham.Skill.Types as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Helpers.Customization
import Arkham.Helpers.Window (checkAfter, checkWindows)
import Arkham.Placement
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window
import Data.IntMap.Strict qualified as IntMap

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    IncreaseCustomization iid cardCode customization choices | toCardCode a == cardCode && a.owner == iid -> do
      case customizationIndex a customization of
        Nothing -> pure a
        Just i ->
          pure
            $ a
              { skillCustomizations = IntMap.adjust (second (const choices) . first (+ 1)) i (skillCustomizations a)
              }
    SealedChaosToken token (isTarget a -> True) -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    SealedChaosToken token _ -> do
      pure $ a & sealedChaosTokensL %~ filter (/= token)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    UseCardAbility _ (isSource a -> True) (-1) _ payment -> do
      pure $ a {skillAdditionalPayment = Just payment}
    InvestigatorCommittedSkill _ skillId | skillId == toId a -> do
      pure $ a {skillPlacement = Limbo}
    PlaceSkill sid placement | sid == toId a -> do
      for_ placement.attachedTo \target ->
        pushM $ checkAfter $ Window.AttachCard (Just a.controller) (toCard a) target
      pure $ a {skillPlacement = placement}
    RemoveAllAttachments source target -> do
      case placementToAttached a.placement of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
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
        : [UnsealChaosToken token | token <- skillSealedChaosTokens a]
          <> [RemovedFromPlay source]
      pure a
    _ -> pure a
