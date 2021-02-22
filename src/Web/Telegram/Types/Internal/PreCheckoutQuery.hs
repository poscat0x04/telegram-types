module Web.Telegram.Types.Internal.PreCheckoutQuery where

import Common
import Web.Telegram.Types.Internal.OrderInfo
import Web.Telegram.Types.Internal.User

-- | Information about an incoming pre-checkout query
data PreCheckoutQuery = PreCheckoutQuery
  { -- | Unique query identifier
    id :: Text,
    -- | User who sent the query
    from :: User,
    -- | Three-letter ISO 4217 [currency](https://core.telegram.org/bots/payments#supported-currencies) code
    currency :: Text,
    -- | Total price in the smallest units of the currency (integer, not float/double).
    --   For example, for a price of @US$ 1.45@ pass @amount = 145@. See the exp parameter
    --   in [currencies.json](https://core.telegram.org/bots/payments/currencies.json),
    --   it shows the number of digits past the decimal point for each currency
    --   (2 for the majority of currencies).
    totalAmount :: Int,
    -- | Bot specified invoice payload
    invoicePayload :: Text,
    -- | Identifier of the shipping option chosen by the user
    shippingOptionId :: Maybe Text,
    -- | Order info provided by the user
    orderInfo :: Maybe OrderInfo
  }
  deriving stock (Show, Eq)

mkLabel ''PreCheckoutQuery
deriveJSON snake ''PreCheckoutQuery
