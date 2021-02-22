module Web.Telegram.Types.Internal.PassportData where

import Common
import Web.Telegram.Types.Internal.EncryptedCredentials
import Web.Telegram.Types.Internal.EncryptedPassportElement

-- | Information about Telegram Passport data shared with the bot by the user.
data PassportData = PassportData
  { -- | Array with information about documents and other
    --   Telegram Passport elements that was shared with the bot
    _data :: [EncryptedPassportElement],
    -- | Encrypted credentials required to decrypt the data
    credentials :: EncryptedCredentials
  }
  deriving stock (Show, Eq)

mkLabel ''PassportData
deriveJSON snake ''PassportData
