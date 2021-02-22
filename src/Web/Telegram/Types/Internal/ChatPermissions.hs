module Web.Telegram.Types.Internal.ChatPermissions where

import Common

-- | Actions that a non-administrator user is allowed to take in a chat
data ChatPermissions = ChatPermissions
  { -- | True, if the user is allowed to send text messages, contacts, locations and venues
    canSendMessages :: Maybe Bool,
    -- | True, if the user is allowed to send audios, documents, photos, videos, video
    --   notes and voice notes, implies 'canSendMessages'
    canSendMediaMessages :: Maybe Bool,
    -- | True, if the user is allowed to send polls, implies 'canSendMessages'
    canSendPolls :: Maybe Bool,
    -- | True, if the user is allowed to send animations, games, stickers and use inline
    --   bots, implies 'canSendMediaMessages'
    canSendOtherMesssages :: Maybe Bool,
    -- | True, if the user is allowed to add web page previews to their messages, implies
    --   'canSendMediaMessages'
    canAddWebPagePreviews :: Maybe Bool,
    -- | True, if the user is allowed to change the chat title, photo and other settings.
    --   Ignored in public supergroups
    canChangeInfo :: Maybe Bool,
    -- | True, if the user is allowed to invite new users to the chat
    canInviteUsers :: Maybe Bool,
    -- | True, if the user is allowed to pin messages. Ignored in public supergroups
    canPinMessages :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''ChatPermissions
deriveJSON snake ''ChatPermissions
