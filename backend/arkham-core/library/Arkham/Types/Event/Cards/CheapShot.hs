module Arkham.Types.Event.Cards.CheapShot
  ( cheapShot
  , CheapShot(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype CheapShot = CheapShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot :: EventCard CheapShot
cheapShot = event CheapShot Cards.cheapShot

instance EventRunner env => RunMessage env CheapShot where
  runMessage msg e@(CheapShot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      agility <- getSkillValue SkillAgility iid
      e <$ pushAll
        [ CreateEffect
          (cdCardCode $ toCardDef attrs)
          (Just $ EffectInt agility)
          (toSource attrs)
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        , Discard (toTarget attrs)
        ]
    PassedSkillTest iid _ _ (SkillTestInitiatorTarget (InvestigatorTarget _)) _ n
      | n >= 2
      -> do
        mSkillTestTarget <- getSkillTestTarget
        e <$ case mSkillTestTarget of
          Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
          _ -> pure ()
    _ -> CheapShot <$> runMessage msg attrs
