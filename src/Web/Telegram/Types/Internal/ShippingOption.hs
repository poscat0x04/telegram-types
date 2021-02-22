module Web.Telegram.Types.Internal.ShippingOption where

import Common
import Web.Telegram.Types.Internal.LabeledPrice

-- | One shipping option
data ShippingOption = ShippingOption
  { -- | Shipping option identifier
    id :: Text,
    -- | Option title
    title :: Text,
    -- | List of price portions
    prices :: [LabeledPrice]
  }
  deriving stock (Show, Eq)

mkLabel ''ShippingOption
deriveJSON snake ''ShippingOption
