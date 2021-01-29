module Web.Telegram.Types.Internal.MessageEntity where

import Common
import Web.Telegram.Types.Internal.User

data MessageEntityType
  = Mention
  | Hashtag
  | Cashtag
  | BotCommand
  | Url
  | Email
  | PhoneNumber
  | Bold
  | Italic
  | Underline
  | Strikethrough
  | Code
  | Pre
  | TextLink
  | TextMention
  deriving stock (Show, Eq, Ord, Enum)

data MessageEntity = MessageEntity
  { _type :: MessageEntityType,
    offset :: Int,
    length :: Int,
    url :: Maybe Text,
    user :: Maybe User,
    language :: Maybe Text
  }
  deriving stock (Show, Eq)

makePrismLabels ''MessageEntityType
mkLabel ''MessageEntity
deriveJSON sumSnake ''MessageEntityType
deriveJSON snake ''MessageEntity
