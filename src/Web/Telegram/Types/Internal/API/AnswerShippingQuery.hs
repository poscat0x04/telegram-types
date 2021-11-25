module Web.Telegram.Types.Internal.API.AnswerShippingQuery where

import Common
import Web.Telegram.Types.Internal.ShippingOption

data AnswerShippingQuery = AnswerShippingQuery
  { shippingQueryId :: Text,
    ok :: Bool,
    shippingOptions :: Maybe [ShippingOption],
    errorMessage :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''AnswerShippingQuery
deriveToJSON snake ''AnswerShippingQuery
makeMethod ''AnswerShippingQuery
