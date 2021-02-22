module Web.Telegram.Types.Internal.ChatMember where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.User

data MemberStatus
  = Creator
  | Administrator
  | Member
  | Restricted
  | Left
  | Kicked
  deriving stock (Show, Eq)

-- | Information about one member of a chat.
data ChatMember = ChatMember
  { -- | Information about the user
    user :: User,
    -- | The member's status in the chat
    status :: MemberStatus,
    -- | Owner and administrators only. Custom title for this user
    customTitle :: Maybe Text,
    -- | Owner and administrators only. True, if the user's presence in the chat is hidden
    isAnonymous :: Maybe Bool,
    -- | Administrators only. True, if the bot is allowed to edit
    --   administrator privileges of that user
    canBeEdited :: Maybe Bool,
    -- | Administrators only. True, if the administrator can post in the channel; channels only
    canPostMessages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can edit messages of other users and
    --   can pin messages; channels only
    canEditMessages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can delete messages of other users
    canDeleteMessages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can restrict, ban or unban chat members
    canRestrictMembers :: Maybe Bool,
    -- | Administrators only. True, if the administrator can add new administrators with a subset
    --   of their own privileges or demote administrators that he has promoted, directly or
    --   indirectly (promoted by administrators that were appointed by the user)
    canPromoteMembers :: Maybe Bool,
    -- | Administrators and restricted only. True, if the user is allowed to change the chat
    --   title, photo and other settings
    canChangeInfo :: Maybe Bool,
    -- | Administrators and restricted only. True, if the user is allowed to invite new
    --   users to the chat
    canInviteUsers :: Maybe Bool,
    -- | Administrators and restricted only. True, if the user is allowed to pin messages;
    --   groups and supergroups only
    canPinMessages :: Maybe Bool,
    -- | Restricted only. True, if the user is a member of the chat at the moment of the request
    isMember :: Maybe Bool,
    -- | Restricted only. True, if the user is allowed to send text messages, contacts,
    --   locations and venues
    canSendMessages :: Maybe Bool,
    -- | Restricted only. True, if the user is allowed to send audios, documents, photos,
    --   videos, video notes and voice notes
    canSendMediaMessages :: Maybe Bool,
    -- | Restricted only. True, if the user is allowed to send polls
    canSendPolls :: Maybe Bool,
    -- | Restricted only. True, if the user is allowed to send animations, games, stickers and
    --   use inline bots
    canSendOtherMesssages :: Maybe Bool,
    -- | Restricted only. True, if the user is allowed to add web page previews to their messages
    canAddWebPagePreviews :: Maybe Bool,
    -- | Restricted and kicked only. Date when restrictions will be lifted for this user; unix time
    untilDate :: Maybe POSIXTime
  }
  deriving stock (Show, Eq)

makePrismLabels ''MemberStatus
mkLabel ''ChatMember
deriveJSON sumSnake ''MemberStatus
deriveJSON snake ''ChatMember
