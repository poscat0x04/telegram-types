{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Web.Telegram.Types.Internal.UpdateType where

import Deriving.Aeson
import Web.Telegram.Types.Internal.Utils

data UpdateType
  = Message
  | EditedMessage
  | ChannelPost
  | EditedChannelPost
  | InlineQuery
  | ChosenInlineResult
  | CallbackQuery
  | ShippingQuery
  | PreCheckoutQuery
  | PollUpdate
  | PollAnswer
  deriving (Show, Eq, Generic, Default, Enum)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, ConstructorTagModifier CamelToSnake] UpdateType
