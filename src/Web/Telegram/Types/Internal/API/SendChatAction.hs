module Web.Telegram.Types.Internal.API.SendChatAction where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data Action
  = Typing
  | UploadPhoto
  | RecordVideo
  | UploadVideo
  | RecordAudio
  | UploadAudio
  | UploadDocument
  | FindLocation
  | RecordVideoNote
  | UploadVideoNote
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data SendChatAction = SendChatAction
  { chatId :: ChatId,
    action :: Action
  }
  deriving stock (Show, Eq)

makePrismLabels ''Action
mkLabel ''SendChatAction
deriveToJSON sumSnake ''Action
deriveToJSON snake ''SendChatAction
