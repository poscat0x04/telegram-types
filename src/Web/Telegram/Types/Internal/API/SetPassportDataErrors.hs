module Web.Telegram.Types.Internal.API.SetPassportDataErrors where

import Common
import Web.Telegram.Types.Internal.PassportElementError

data SetPassportDataErrors = SetPassportDataErrors
  { userId :: Int,
    errors :: [PassportElementError]
  }
  deriving stock (Show, Eq)

mkLabel ''SetPassportDataErrors
deriveToJSON snake ''SetPassportDataErrors
