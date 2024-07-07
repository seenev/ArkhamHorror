module Arkham.Skill.Cards.Enraptured (
  enraptured,
  Enraptured (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Uses qualified as Uses
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Enraptured = Enraptured SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enraptured :: SkillCard Enraptured
enraptured = skill Enraptured Cards.enraptured

instance RunMessage Enraptured where
  runMessage msg s@(Enraptured attrs) = case msg of
    PassedSkillTest _ (Just Action.Investigate) _ (isTarget attrs -> True) _ _ ->
      do
        chargeAssets <-
          select
            $ assetControlledBy (skillOwner attrs)
            <> AssetWithUseType Uses.Charge
        secretAssets <-
          select
            $ assetControlledBy (skillOwner attrs)
            <> AssetWithUseType Uses.Secret
        player <- getPlayer (skillOwner attrs)
        unless (null chargeAssets && null secretAssets)
          $ push
          $ chooseOne player
          $ [ targetLabel aid [AddUses (toSource attrs) aid Uses.Charge 1]
            | aid <- chargeAssets
            ]
          <> [ targetLabel aid [AddUses (toSource attrs) aid Uses.Secret 1]
             | aid <- secretAssets
             ]
        pure s
    _ -> Enraptured <$> runMessage msg attrs
