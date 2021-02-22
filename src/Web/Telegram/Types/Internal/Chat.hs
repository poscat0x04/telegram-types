module Web.Telegram.Types.Internal.Chat where

import Common
import Web.Telegram.Types.Internal.ChatLocation
import Web.Telegram.Types.Internal.ChatPermissions
import Web.Telegram.Types.Internal.ChatPhoto
import Web.Telegram.Types.Internal.Message

data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving stock (Show, Eq, Ord, Enum)

data Chat = Chat
  { -- | Unique identifier for this chat.
    id :: Int,
    _type :: ChatType,
    -- | Title, for supergroups, channels and group chats
    title :: Maybe Text,
    -- | Username, for private chats, supergroups and channels if available
    username :: Maybe Text,
    -- | First name of the other party in a private chat
    firstName :: Maybe Text,
    -- | Last name of the other party in a private chat
    lastName :: Maybe Text,
    -- | Chat photo. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    photo :: Maybe ChatPhoto,
    -- | Bio of the other party in a private chat. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    bio :: Maybe Text,
    -- | Description, for groups, supergroups and channel chats. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    description :: Maybe Text,
    -- | Chat invite link, for groups, supergroups and channel chats.
    --   Each administrator in a chat generates their own invite links,
    --   so the bot must first generate the link using exportChatInviteLink.
    --   Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    inviteLink :: Maybe Text,
    -- | The most recent pinned message (by sending date). Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    pinnedMessage :: Maybe Message,
    -- | Default chat member permissions, for groups and supergroups. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    permissions :: Maybe ChatPermissions,
    -- | For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user.
    --   Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    slowModeDelay :: Maybe Int,
    -- | For supergroups, name of group sticker set. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    stickerSetName :: Maybe Int,
    -- | True, if the bot can change the group sticker set. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    canSetStickerSet :: Maybe Bool,
    -- | Unique identifier for the linked chat, i.e. the discussion group identifier
    --   for a channel and vice versa; for supergroups and channel chats.
    --   This identifier may be greater than 32 bits and some programming languages
    --   may have difficulty/silent defects in interpreting it. But it is smaller than
    --   52 bits, so a signed 64 bit integer or double-precision float type are safe
    --   for storing this identifier. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    linkedChatId :: Maybe Int,
    -- | For supergroups, the location to which the supergroup is connected. Returned only in [getChat](https://core.telegram.org/bots/api#getchat).
    location :: Maybe ChatLocation
  }
  deriving stock (Show, Eq)

makePrismLabels ''ChatType
mkLabel ''Chat
deriveJSON sumSnake ''ChatType
deriveJSON snake ''Chat
