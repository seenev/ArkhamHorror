module Arkham.Treachery.Cards.FinalRhapsody where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FinalRhapsody = FinalRhapsody TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsody :: TreacheryCard FinalRhapsody
finalRhapsody = treachery FinalRhapsody Cards.finalRhapsody

instance RunMessage FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure t
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      let damageCount = count ((`elem` [Skull, AutoFail]) . chaosTokenFace) tokens
      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            [ Label
                ("Take " <> tshow damageCount <> " damage and horror")
                [assignDamageAndHorror iid source damageCount damageCount]
            ]
        , ResetChaosTokens source
        ]
      pure t
    _ -> FinalRhapsody <$> runMessage msg attrs
