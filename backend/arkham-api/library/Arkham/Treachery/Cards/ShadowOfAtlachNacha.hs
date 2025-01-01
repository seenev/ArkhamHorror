module Arkham.Treachery.Cards.ShadowOfAtlachNacha (shadowOfAtlachNacha, ShadowOfAtlachNacha (..)) where

import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShadowOfAtlachNacha = ShadowOfAtlachNacha TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowOfAtlachNacha :: TreacheryCard ShadowOfAtlachNacha
shadowOfAtlachNacha = treachery ShadowOfAtlachNacha Cards.shadowOfAtlachNacha

instance RunMessage ShadowOfAtlachNacha where
  runMessage msg t@(ShadowOfAtlachNacha attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest
        sid
        iid
        attrs
        #willpower
        (SumCalculation [Fixed 2, ScenarioCount Distortion])
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamageAndHorror iid attrs 1 1
      pure t
    _ -> ShadowOfAtlachNacha <$> liftRunMessage msg attrs
