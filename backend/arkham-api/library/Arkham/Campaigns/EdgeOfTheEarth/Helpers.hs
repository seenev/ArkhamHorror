module Arkham.Campaigns.EdgeOfTheEarth.Helpers where

import Arkham.I18n
import Arkham.Prelude

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "edgeOfTheEarth" a
