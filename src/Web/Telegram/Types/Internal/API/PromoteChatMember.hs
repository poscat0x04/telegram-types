module Web.Telegram.Types.Internal.API.PromoteChatMember where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data PromoteChatMember = PromoteChatMember
  { chatId :: ChatId,
    userId :: Int,
    canChangeInfo :: Maybe Bool,
    canPostMessages :: Maybe Bool,
    canEditMessages :: Maybe Bool,
    canDeleteMessages :: Maybe Bool,
    canInviteUsers :: Maybe Bool,
    canRestrictMembers :: Maybe Bool,
    canPinMessages :: Maybe Bool,
    canPromoteMembers :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''PromoteChatMember
deriveToJSON snake ''PromoteChatMember
makeMethod ''PromoteChatMember
