{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.Common where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.Passport
import Web.Telegram.Types.Internal.Payment
import Web.Telegram.Types.Internal.Poll
import Web.Telegram.Types.Internal.Sticker
import Web.Telegram.Types.Internal.User

data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving stock (Show, Eq, Ord, Enum)

data Chat = Chat
  { id :: Int,
    _type :: ChatType,
    title :: Maybe Text,
    username :: Maybe Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    photo :: Maybe ChatPhoto,
    description :: Maybe Text,
    inviteLink :: Maybe Text,
    pinnedMessage :: Maybe Message,
    permissions :: Maybe ChatPermissions,
    slowModeDelay :: Maybe Int,
    stickerSetName :: Maybe Int,
    canSetStickerSet :: Maybe Bool,
    linkedChatId :: Maybe Int,
    location :: Maybe ChatLocation
  }
  deriving stock (Show, Eq)

data ChatPermissions = ChatPermissions
  { canSendMessages :: Maybe Bool,
    canSendMediaMessages :: Maybe Bool,
    canSendPolls :: Maybe Bool,
    canSendOtherMesssages :: Maybe Bool,
    canAddWebPagePreviews :: Maybe Bool,
    canChangeInfo :: Maybe Bool,
    canInviteUsers :: Maybe Bool,
    canPinMessages :: Maybe Bool
  }
  deriving stock (Show, Eq)

data ChatLocation = ChatLocation
  { location :: Location,
    address :: Text
  }
  deriving stock (Show, Eq)

data MemberStatus
  = Creator
  | Administrator
  | Member
  | Restricted
  | Left
  | Kicked
  deriving (Show, Eq)

data ChatMember = ChatMember
  { user :: User,
    status :: MemberStatus,
    customTitle :: Maybe Text,
    isAnonymous :: Maybe Bool,
    untilDate :: Maybe POSIXTime,
    canBeEdited :: Maybe Bool,
    canPostMessages :: Maybe Bool,
    canEditMessages :: Maybe Bool,
    canDeleteMessages :: Maybe Bool,
    canRestrictMembers :: Maybe Bool,
    canPromoteMembers :: Maybe Bool,
    canChangeInfo :: Maybe Bool,
    canInviteUsers :: Maybe Bool,
    canPinMessages :: Maybe Bool,
    isMember :: Maybe Bool,
    canSendMessages :: Maybe Bool,
    canSendMediaMessages :: Maybe Bool,
    canSendPolls :: Maybe Bool,
    canSendOtherMesssages :: Maybe Bool,
    canAddWebPagePreviews :: Maybe Bool
  }
  deriving stock (Show, Eq)

data Message = Message
  { metadata :: MessageMetadata,
    content :: MessageContent
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten Message instance FromJSON Message

deriving via Flatten Message instance ToJSON Message

data MessageMetadata = MessageMetadata
  { messageId :: Int,
    from :: Maybe User,
    senderChat :: Maybe Chat,
    date :: POSIXTime,
    chat :: Chat,
    forwardFrom :: Maybe User,
    forwardFromChat :: Maybe Chat,
    forwardFromMessageId :: Maybe Int,
    forwardSignature :: Maybe Text,
    forwardSenderName :: Maybe Text,
    forwardDate :: Maybe POSIXTime,
    replyToMessage :: Maybe Message,
    viaBot :: Maybe User,
    editDate :: Maybe POSIXTime,
    mediaGroupId :: Maybe Text,
    authorSignature :: Maybe Text,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

data MessageContent
  = TextMessage
      { text :: Text,
        entities :: Maybe [MessageEntity]
      }
  | AudioMessage
      { audio :: Audio,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | DocumentMessage
      { document :: Document,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | AnimationMessage
      { animation :: Animation
      }
  | GameMessage
      { game :: Game
      }
  | PhotoMessage
      { photo :: [PhotoSize],
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | StickerMessage
      { sticker :: Sticker
      }
  | VideoMessage
      { video :: Video,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | VoiceMessage
      { voice :: Voice,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | VideoNoteMessage
      { videoNote :: VideoNote
      }
  | ContactMessage
      { contact :: Contact
      }
  | DiceMessage
      { dice :: Dice
      }
  | LocationMessage
      { location :: Location
      }
  | VenueMessage
      { venue :: Venue
      }
  | PollMessage
      { poll :: Poll
      }
  | NewChatMembers
      { newChatMembers :: [User]
      }
  | LeftChatMember
      { leftChatMember :: User
      }
  | NewChatPhoto
      { newChatPhoto :: [PhotoSize]
      }
  | DeleteChatPhoto
      { deleteChatPhoto :: Bool
      }
  | GroupChatCreated
      { groupChatCreated :: Bool
      }
  | SupergroupChatCreated
      { supergroupChatCreated :: Bool
      }
  | ChannelChatCreated
      { channelChatCreated :: Bool
      }
  | MigrateToChatId
      { migrateToChatId :: Int
      }
  | MigrateFromChatId
      { migrateFromChatId :: Int
      }
  | PinnedMessage
      { pinnedMessage :: Message
      }
  | InvoiceMessage
      { invoice :: Invoice
      }
  | SuccessfulPaymentMessage
      { successfulPayment :: SuccessfulPayment
      }
  | ConnectedWebsite
      { connectedWebsite :: Text
      }
  | PassportData
      { passportData :: PassportData
      }
  | ProximityAlertTriggeredMessage
      { proximityAlertTriggered :: ProximityAlertTriggered
      }
  deriving stock (Show, Eq)

data CallbackQuery = CallbackQuery
  { id :: Text,
    from :: User,
    message :: Message,
    inlineMessageId :: Maybe Text,
    chatInstance :: Text,
    _data :: Maybe Text,
    gameShortName :: Maybe Text
  }
  deriving stock (Show, Eq)

data ForceReply = ForceReply
  { forceReply :: Bool,
    selective :: Maybe Bool
  }
  deriving stock (Show, Eq)

makePrismLabels ''ChatType
mkLabel ''Chat
mkLabel ''ChatPermissions
mkLabel ''ChatLocation
makePrismLabels ''MemberStatus
mkLabel ''ChatMember
mkLabel ''Message
mkLabel ''MessageMetadata
mkLabel ''MessageContent
mkLabel ''CallbackQuery
mkLabel ''ForceReply
deriveJSON sumSnake ''ChatType
deriveJSON snake ''Chat
deriveJSON snake ''ChatPermissions
deriveJSON snake ''ChatLocation
deriveJSON snake ''MessageMetadata
deriveJSON sumSnake ''MessageContent
deriveJSON snake ''CallbackQuery
deriveJSON snake ''ForceReply
