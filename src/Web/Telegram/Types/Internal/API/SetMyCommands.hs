module Web.Telegram.Types.Internal.API.SetMyCommands where

import Common
import Web.Telegram.Types.Internal.BotCommand

newtype SetMyCommands = SetMyCommands
  {commands :: [BotCommand]}
  deriving stock (Show, Eq)

mkLabel ''SetMyCommands
deriveToJSON snake ''SetMyCommands
makeMethod ''SetMyCommands
