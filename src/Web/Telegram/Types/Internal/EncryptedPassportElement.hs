module Web.Telegram.Types.Internal.EncryptedPassportElement where

import Common
import Web.Telegram.Types.Internal.PassportFile

data EncryptedPassportElementType
  = PersonalDetails
  | Passport
  | DriverLicense
  | IdentityCard
  | InternalPassport
  | Address
  | UtilityBill
  | BankStatement
  | RentalAgreement
  | PassportRegistration
  | TemporaryRegistration
  | PhoneNumber
  | Email
  deriving stock (Show, Eq, Ord, Enum)

data EncryptedPassportElement
  = PersonalDetailsElement
      { -- | Base64-encoded element hash for using in
        --   'Web.Telegram.Types.Internal.PassportElementError.PassportElementErrorUnspecified'
        hash :: Text,
        -- | Base64-encoded encrypted Telegram Passport element data provided by the user.
        --   Can be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        _data :: Text
      }
  | PassportElement
      { hash :: Text,
        _data :: Text,
        -- | Encrypted file with the front side of the document
        --   The file be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        frontSide :: PassportFile,
        -- | Encrypted file with the selfie of the user holding a document
        --   The file be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        selfie :: PassportFile,
        -- | Array of encrypted files with translated versions of documents provided by the user
        --   Files can be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        translation :: [PassportFile]
      }
  | DriverLicenseElement
      { hash :: Text,
        _data :: Text,
        frontSide :: PassportFile,
        -- | Encrypted file with the reverse side of the document
        --   The file be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        reverseSide :: PassportFile,
        selfie :: PassportFile,
        translation :: [PassportFile]
      }
  | IdentityCardElement
      { hash :: Text,
        _data :: Text,
        frontSide :: PassportFile,
        reverseSide :: PassportFile,
        selfie :: PassportFile,
        translation :: [PassportFile]
      }
  | InternalPassportElement
      { hash :: Text,
        _data :: Text,
        frontSide :: PassportFile,
        selfie :: PassportFile,
        translation :: [PassportFile]
      }
  | AddressElement
      { hash :: Text,
        _data :: Text
      }
  | UtilityBillElement
      { hash :: Text,
        -- | Array of encrypted files with documents provided by the user
        --   Files can be decrypted and verified using the accompanying
        --   'Web.Telegram.Types.Internal.EncryptedCredentials.EncryptedCredentials'
        files :: [PassportFile],
        translation :: [PassportFile]
      }
  | BankStatementElement
      { hash :: Text,
        files :: [PassportFile],
        translation :: [PassportFile]
      }
  | RentalAgreementElement
      { hash :: Text,
        files :: [PassportFile],
        translation :: [PassportFile]
      }
  | PassportRegistrationElement
      { hash :: Text,
        files :: [PassportFile],
        translation :: [PassportFile]
      }
  | TemporaryRegistrationElement
      { hash :: Text,
        files :: [PassportFile],
        translation :: [PassportFile]
      }
  | PhoneNumberElement
      { hash :: Text,
        -- | User's verified phone number
        phoneNumber :: Text
      }
  | EmailElement
      { hash :: Text,
        -- | User's verified email address
        email :: Text
      }
  deriving stock (Show, Eq)

makePrismLabels ''EncryptedPassportElementType
mkLabel ''EncryptedPassportElement
deriveJSON sumSnake ''EncryptedPassportElementType
deriveJSON
  ( defaultOptions
      { sumEncoding = TaggedObject "type" undefined,
        constructorTagModifier = camelTo2 '_' . tryStripSuffix "Element",
        fieldLabelModifier = camelTo2 '_' . tryStrip "_"
      }
  )
  ''EncryptedPassportElement
