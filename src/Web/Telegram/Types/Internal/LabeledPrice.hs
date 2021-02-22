module Web.Telegram.Types.Internal.LabeledPrice where

import Common

-- | A portion of the price for goods or services
data LabeledPrice = LabeledPrice
  { -- | Portion label
    label :: Text,
    -- | Total price in the smallest units of the currency (integer, not float/double).
    --   For example, for a price of @US$ 1.45@ pass @amount = 145@. See the exp parameter
    --   in [currencies.json](https://core.telegram.org/bots/payments/currencies.json),
    --   it shows the number of digits past the decimal point for each currency
    --   (2 for the majority of currencies).
    amount :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''LabeledPrice
deriveJSON snake ''LabeledPrice
