module Arkham.Helpers.Ability where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.HasAbilities

withBaseAbilities :: HasAbilities a => a -> [Ability] -> [Ability]
withBaseAbilities a f = getAbilities a <> f

extend :: HasAbilities a => a -> [Ability] -> [Ability]
extend = withBaseAbilities

extend1 :: HasAbilities a => a -> Ability -> [Ability]
extend1 a ab = withBaseAbilities a [ab]
