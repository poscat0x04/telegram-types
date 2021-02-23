module Web.Telegram.Types.Internal.API.GetFile where

import Common

newtype GetFile = GetFile
  {fileId :: Text}
  deriving stock (Show, Eq)

mkLabel ''GetFile
deriveToJSON snake ''GetFile
