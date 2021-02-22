module Web.Telegram.Types.Internal.MessageId where

import Common

newtype MessageId = MessageId
  { -- | Unique message identifier
    messageId :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''MessageId
deriveJSON snake ''MessageId
