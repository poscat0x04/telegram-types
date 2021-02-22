module Web.Telegram.Types.Internal.ShippingAddress where

import Common

-- | A shipping address
data ShippingAddress = ShippingAddress
  { -- | ISO 3166-1 alpha-2 country code
    countryCode :: Text,
    -- | State, if applicable
    state :: Text,
    -- | City
    city :: Text,
    -- | First line for the address
    streetLine1 :: Text,
    -- | Second line for the address
    streetLine2 :: Text,
    -- | Address post code
    postCode :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''ShippingAddress
deriveJSON snake ''ShippingAddress
