module Web.Telegram.Types.Internal.EncryptedCredentials where

import Common

-- | Data required for decrypting and authenticating
--   'Web.Telegram.Types.Internal.EncryptedPassportElement.EncryptedPassportElement'.
--   See the [Telegram Passport Documentation](https://core.telegram.org/passport#receiving-information)
--   for a complete description of the data decryption and authentication processes.
data EncryptedCredentials = EncryptedCredentials
  { -- | Base64-encoded encrypted JSON-serialized data with unique user's payload,
    --   data hashes and secrets required for
    --   'Web.Telegram.Types.Internal.EncryptedPassportElement.EncryptedPassportElement'.
    --   decryption and authentication
    _data :: Text,
    -- | Base64-encoded data hash for data authentication
    hash :: Text,
    -- | Base64-encoded secret, encrypted with the bot's public RSA key, required for data decryption
    secret :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''EncryptedCredentials
deriveJSON snake ''EncryptedCredentials
