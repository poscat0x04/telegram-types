module Web.Telegram.Types.Internal.Payment where

import Common
import Web.Telegram.Types.Internal.User

data Invoice = Invoice
  { title :: Text,
    description :: Text,
    startParameter :: Text,
    currency :: Text,
    totalAmount :: Int
  }
  deriving stock (Show, Eq)

data ShippingAddress = ShippingAddress
  { countryCode :: Text,
    state :: Text,
    city :: Text,
    streetLine1 :: Text,
    streetLine2 :: Text,
    postCode :: Text
  }
  deriving stock (Show, Eq)

data OrderInfo = OrderInfo
  { name :: Maybe Text,
    phoneNumber :: Maybe Text,
    email :: Maybe Text,
    shippingAddress :: ShippingAddress
  }
  deriving stock (Show, Eq)

data ShippingOption = ShippingOption
  { id :: Text,
    title :: Text,
    prices :: Text
  }
  deriving stock (Show, Eq)

data SuccessfulPayment = SuccessfulPayment
  { currency :: Text,
    totalAmount :: Int,
    invoicePayload :: Text,
    shippingOptionId :: Maybe Text,
    orderInfo :: Maybe OrderInfo,
    telegramPaymentChargeId :: Text,
    providerPaymentChargeId :: Text
  }
  deriving stock (Show, Eq)

data ShippingQuery = ShippingQuery
  { id :: Text,
    from :: User,
    invoicePayload :: Text,
    shippingAddress :: ShippingAddress
  }
  deriving stock (Show, Eq)

data PreCheckoutQuery = PreCheckoutQuery
  { id :: Text,
    from :: User,
    currency :: Text,
    totalAmount :: Int,
    invoicePayload :: Text,
    shippingOptionId :: Maybe Text,
    orderInfo :: Maybe OrderInfo
  }
  deriving stock (Show, Eq)

mkLabel ''Invoice
mkLabel ''ShippingAddress
mkLabel ''OrderInfo
mkLabel ''ShippingOption
mkLabel ''SuccessfulPayment
mkLabel ''ShippingQuery
mkLabel ''PreCheckoutQuery
deriveJSON snake ''Invoice
deriveJSON snake ''ShippingAddress
deriveJSON snake ''OrderInfo
deriveJSON snake ''ShippingOption
deriveJSON snake ''SuccessfulPayment
deriveJSON snake ''ShippingQuery
deriveJSON snake ''PreCheckoutQuery
