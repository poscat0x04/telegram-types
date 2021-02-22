module Web.Telegram.Types.Internal.ShippingQuery where

import Common
import Web.Telegram.Types.Internal.ShippingAddress
import Web.Telegram.Types.Internal.User

data ShippingQuery = ShippingQuery
  { id :: Text,
    from :: User,
    invoicePayload :: Text,
    shippingAddress :: ShippingAddress
  }
  deriving stock (Show, Eq)

mkLabel ''ShippingQuery
deriveJSON snake ''ShippingQuery
