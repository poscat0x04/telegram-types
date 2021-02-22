module Web.Telegram.Types.Internal.MessageEntity where

import Common
import Web.Telegram.Types.Internal.User

-- | Type of the entity
data MessageEntityType
  = -- | @@username@
    Mention
  | -- | @#hashtag@
    Hashtag
  | -- | @$USD@
    Cashtag
  | -- | @/start@jobs_bot@
    BotCommand
  | -- | @https://telegram.org@
    Url
  | -- | @do-not-reply@telegram.org@
    Email
  | -- | @+1-212-555-0123@
    PhoneNumber
  | Bold
  | Italic
  | Underline
  | Strikethrough
  | -- | monospaced string
    Code
  | -- | monospaced block
    Pre
  | -- | clickable text URLs
    TextLink
  | -- | for users without usernames
    TextMention
  deriving stock (Show, Eq, Ord, Enum)

data MessageEntity = MessageEntity
  { _type :: MessageEntityType,
    -- | Offset in UTF-16 code units to the start of the entity
    offset :: Int,
    -- | Length of the entity in UTF-16 code units
    length :: Int,
    -- | For "text_link" only, url that will be opened after user taps on the text
    url :: Maybe Text,
    -- | For "text_mention" only, the mentioned user
    user :: Maybe User,
    -- | For "pre" only, the programming language of the entity text
    language :: Maybe Text
  }
  deriving stock (Show, Eq)

makePrismLabels ''MessageEntityType
mkLabel ''MessageEntity
deriveJSON sumSnake ''MessageEntityType
deriveJSON snake ''MessageEntity
