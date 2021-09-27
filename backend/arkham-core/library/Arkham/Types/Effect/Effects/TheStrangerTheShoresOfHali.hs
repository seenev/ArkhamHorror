module Arkham.Types.Effect.Effects.TheStrangerTheShoresOfHali
  ( TheStrangerTheShoresOfHali(..)
  , theStrangerTheShoresOfHali
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali EffectAttrs
  deriving anyclass (HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: EffectArgs -> TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  TheStrangerTheShoresOfHali . uncurry4 (baseAttrs "03047a")

instance HasAbilities TheStrangerTheShoresOfHali where
  getAbilities (TheStrangerTheShoresOfHali attrs) =
    [ mkAbility
          (ProxySource
            (LocationMatcherSource LocationWithAnyHorror)
            (toSource attrs)
          )
          1
          (ForcedAbility $ Leaves Timing.When You ThisLocation)
        & abilityLimitL
        .~ PlayerLimit PerRound 1
    ]

instance HasQueue env => RunMessage env TheStrangerTheShoresOfHali where
  runMessage msg e@(TheStrangerTheShoresOfHali attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _ | isSource attrs source ->
      e
        <$ push
             (BeginSkillTest
               iid
               source
               (InvestigatorTarget iid)
               Nothing
               SkillAgility
               2
             )
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> do
        popMessageMatching_ \case
          MoveFrom _ iid' _ -> iid' == iid
          _ -> False
        popMessageMatching_ \case
          MoveTo _ iid' _ -> iid == iid'
          _ -> False
        e <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> TheStrangerTheShoresOfHali <$> runMessage msg attrs
