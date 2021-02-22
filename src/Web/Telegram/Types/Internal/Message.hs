{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.Message where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.Animation
import Web.Telegram.Types.Internal.Audio
import {-# SOURCE #-} Web.Telegram.Types.Internal.Chat
import Web.Telegram.Types.Internal.Contact
import Web.Telegram.Types.Internal.Dice
import Web.Telegram.Types.Internal.Document
import Web.Telegram.Types.Internal.Game
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.Invoice
import Web.Telegram.Types.Internal.Location
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.PassportData
import Web.Telegram.Types.Internal.PhotoSize
import Web.Telegram.Types.Internal.Poll
import Web.Telegram.Types.Internal.ProximityAlertTriggered
import Web.Telegram.Types.Internal.Sticker
import Web.Telegram.Types.Internal.SuccessfulPayment
import Web.Telegram.Types.Internal.User
import Web.Telegram.Types.Internal.Venue
import Web.Telegram.Types.Internal.Video
import Web.Telegram.Types.Internal.VideoNote
import Web.Telegram.Types.Internal.Voice

data Message = Message
  { metadata :: MessageMetadata,
    content :: MessageContent
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten Message instance FromJSON Message

deriving via Flatten Message instance ToJSON Message

data MessageMetadata = MessageMetadata
  { -- | Unique message identifier inside this chat
    messageId :: Int,
    -- | Sender, empty for messages sent to channels
    from :: Maybe User,
    -- | Sender of the message, sent on behalf of a chat. The channel itself
    --   for channel messages. The supergroup itself for messages from anonymous
    --   group administrators. The linked channel for messages automatically
    --   forwarded to the discussion group
    senderChat :: Maybe Chat,
    -- | Date the message was sent in Unix time
    date :: POSIXTime,
    -- | Conversation the message belongs to
    chat :: Chat,
    -- | For forwarded messages, sender of the original message
    forwardFrom :: Maybe User,
    -- | For messages forwarded from channels or from anonymous administrators,
    --   information about the original sender chat
    forwardFromChat :: Maybe Chat,
    -- | For messages forwarded from channels, identifier of the original message in the channel
    forwardFromMessageId :: Maybe Int,
    -- | For messages forwarded from channels, signature of the post author if present
    forwardSignature :: Maybe Text,
    -- | Sender's name for messages forwarded from users who disallow adding a link
    --   to their account in forwarded messages
    forwardSenderName :: Maybe Text,
    -- | For forwarded messages, date the original message was sent in Unix time
    forwardDate :: Maybe POSIXTime,
    -- | For replies, the original message. Note that the Message object in this
    --   field will not contain further 'replyToMessage' fields even if it itself is a reply.
    replyToMessage :: Maybe Message,
    -- | Bot through which the message was sent
    viaBot :: Maybe User,
    -- | Date the message was last edited in Unix time
    editDate :: Maybe POSIXTime,
    -- | The unique identifier of a media message group this message belongs to
    mediaGroupId :: Maybe Text,
    -- | Signature of the post author for messages in channels, or the custom title of an anonymous group administrator
    authorSignature :: Maybe Text,
    -- | Inline keyboard attached to the message. @login_url@ buttons are represented as ordinary @url@ buttons.
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

data MessageContent
  = TextMessage
      { -- | The actual UTF-8 text of the message, 0-4096 characters
        text :: Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the text
        entities :: Maybe [MessageEntity]
      }
  | -- | Message is an audio file
    AudioMessage
      { -- | Information about the audio file
        audio :: Audio,
        -- | Caption for the audio, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is a general file
    DocumentMessage
      { -- | Information about the file
        document :: Document,
        -- | Caption for the document, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is an animation
    AnimationMessage
      { -- | Information about the animation.
        animation :: Animation,
        -- | Caption for the animation, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is a game
    GameMessage
      { -- | Information about the game.
        game :: Game
      }
  | -- | Message is a photo
    PhotoMessage
      { -- | Available sizes of the photo
        photo :: [PhotoSize],
        -- | Caption for the photo, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is a sticker
    StickerMessage
      { -- | Information about the sticker
        sticker :: Sticker
      }
  | -- | Message is a video
    VideoMessage
      { -- | Information about the video
        video :: Video,
        -- | Caption for the video, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is a voice message
    VoiceMessage
      { -- | Information about the file
        voice :: Voice,
        -- | Caption for the voice, 0-1024 characters
        caption :: Maybe Text,
        -- | Special entities like usernames, URLs, bot commands, etc. that appear in the caption
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | Message is a [video note](https://telegram.org/blog/video-messages-and-telescope)
    VideoNoteMessage
      { -- | Information about the video message
        videoNote :: VideoNote
      }
  | -- | Message is a shared contact
    ContactMessage
      { -- | Information about the contact
        contact :: Contact
      }
  | -- | Message is a dice with random value
    DiceMessage
      { dice :: Dice
      }
  | -- | Message is a shared location
    LocationMessage
      { -- | Information about the location
        location :: Location
      }
  | -- | Message is a venue
    VenueMessage
      { -- | Information about the venue
        venue :: Venue
      }
  | -- | Message is a native poll
    PollMessage
      { -- | Information about the poll
        poll :: Poll
      }
  | NewChatMembers
      { -- | New members that were added to the group or supergroup and information
        --   about them (the bot itself may be one of these members)
        newChatMembers :: [User]
      }
  | LeftChatMember
      { -- | A member was removed from the group, information
        --   about them (this member may be the bot itself)
        leftChatMember :: User
      }
  | NewChatTitle
      { -- | A chat title was changed to this value
        newChatTitle :: Text
      }
  | NewChatPhoto
      { -- | A chat photo was change to this value
        newChatPhoto :: [PhotoSize]
      }
  | DeleteChatPhoto
      { -- | Service message: the chat photo was deleted
        deleteChatPhoto :: Bool
      }
  | GroupChatCreated
      { -- | Service message: the group has been created
        groupChatCreated :: Bool
      }
  | SupergroupChatCreated
      { -- | Service message: the supergroup has been created.
        --   This field can't be received in a message coming through updates,
        --   because bot can't be a member of a supergroup when it is created.
        --   It can only be found in 'replyToMessage' if someone replies to a
        --   very first message in a directly created supergroup.
        supergroupChatCreated :: Bool
      }
  | ChannelChatCreated
      { -- | Service message: the channel has been created.
        --   This field can't be received in a message coming through updates,
        --   because bot can't be a member of a channel when it is created.
        --   It can only be found in 'replyToMessage' if someone replies to a
        --   very first message in a channel.
        channelChatCreated :: Bool
      }
  | MigrateToChatId
      { -- | The supergroup has been migrated from a group with the specified identifier.
        migrateToChatId :: Int
      }
  | MigrateFromChatId
      { -- | The supergroup has been migrated from a group with the specified identifier.
        migrateFromChatId :: Int
      }
  | PinnedMessage
      { -- | Specified message was pinned. Note that the Message object in this field
        --   will not contain further 'replyToMessage' fields even if it is itself a reply.
        pinnedMessage :: Message
      }
  | -- | Message is an invoice for a [payment](https://core.telegram.org/bots/api#payments)
    InvoiceMessage
      { -- | Information about the invoice
        invoice :: Invoice
      }
  | -- | Message is a service message about a successful payment
    SuccessfulPaymentMessage
      { -- | Information about the payment
        successfulPayment :: SuccessfulPayment
      }
  | ConnectedWebsite
      { -- | The domain name of the website on which the user has logged in
        connectedWebsite :: Text
      }
  | PassportData
      { -- | Telegram Passport data
        passportData :: PassportData
      }
  | ProximityAlertTriggeredMessage
      { -- | Service message. A user in the chat triggered another user's
        --   proximity alert while sharing Live Location
        proximityAlertTriggered :: ProximityAlertTriggered
      }
  deriving stock (Show, Eq)

mkLabel ''Message
mkLabel ''MessageMetadata
mkLabel ''MessageContent
deriveJSON snake ''MessageMetadata
deriveJSON sumSnake ''MessageContent
