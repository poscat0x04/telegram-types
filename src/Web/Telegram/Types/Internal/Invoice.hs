module Web.Telegram.Types.Internal.Invoice where

import Common

-- | Basic information about an invoice
data Invoice = Invoice
  { -- | Product name
    title :: Text,
    -- | Product description
    description :: Text,
    -- | Unique bot deep-linking parameter that can be used to generate this invoice
    startParameter :: Text,
    -- | Three-letter ISO 4217 [currency](https://core.telegram.org/bots/payments#supported-currencies) code
    currency :: Text,
    -- | Total price in the smallest units of the currency (integer, not float/double).
    --   For example, for a price of @US$ 1.45@ pass @amount = 145@. See the exp parameter
    --   in [currencies.json](https://core.telegram.org/bots/payments/currencies.json),
    --   it shows the number of digits past the decimal point for each currency
    --   (2 for the majority of currencies).
    totalAmount :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''Invoice
deriveJSON snake ''Invoice
