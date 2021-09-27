module Arkham.Types.Treachery.Cards.DrawingTheSign
  ( drawingTheSign
  , DrawingTheSign(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DrawingTheSign = DrawingTheSign TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingTheSign :: TreacheryCard DrawingTheSign
drawingTheSign = treachery DrawingTheSign Cards.drawingTheSign

instance HasModifiersFor env DrawingTheSign where
  getModifiersFor _ (InvestigatorTarget iid) (DrawingTheSign attrs) =
    pure $ toModifiers
      attrs
      [ HandSize (-5) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities DrawingTheSign where
  getAbilities (DrawingTheSign a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance TreacheryRunner env => RunMessage env DrawingTheSign where
  runMessage msg t@(DrawingTheSign attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> DrawingTheSign <$> runMessage msg attrs
