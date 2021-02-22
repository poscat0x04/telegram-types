module Web.Telegram.Types.Internal.OrderInfo where

import Common
import Web.Telegram.Types.Internal.ShippingAddress

-- | Information about an order
data OrderInfo = OrderInfo
  { -- | User name
    name :: Maybe Text,
    -- | User's phone number
    phoneNumber :: Maybe Text,
    -- | User email
    email :: Maybe Text,
    -- | User shipping address
    shippingAddress :: ShippingAddress
  }
  deriving stock (Show, Eq)

mkLabel ''OrderInfo
deriveJSON snake ''OrderInfo
