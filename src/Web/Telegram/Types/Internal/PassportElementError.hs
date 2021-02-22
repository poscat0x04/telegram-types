module Web.Telegram.Types.Internal.PassportElementError where

import Common
import Web.Telegram.Types.Internal.EncryptedPassportElement

-- | An error in the Telegram Passport element which was submitted that should be resolved by the user
data PassportElementError
  = -- | An issue in one of the data fields that was provided by the user.
    --   The error is considered resolved when the field's value changes.
    PassportElementErrorDataField
      { -- | The section of the user's Telegram Passport which has the error,
        --   one of 'PersonalDetails', 'Passport', 'DriverLicense', 'IdentityCard',
        --   'InternalPassport', 'Address'
        _type :: EncryptedPassportElementType,
        -- | Name of the data field which has the error
        fieldName :: Text,
        -- | Base64-encoded data hash
        dataHash :: Text,
        -- | Error message
        message :: Text
      }
  | -- | An issue with the front side of a document.
    --   The error is considered resolved when the file with the front side of the document changes.
    PassportElementErrorFrontSide
      { -- | The section of the user's Telegram Passport which has the issue,
        --   one of 'Passport', 'DriverLicense', 'IdentityCard', 'InternalPassport'
        _type :: EncryptedPassportElementType,
        -- | Base64-encoded hash of the file with the front side of the document
        fileHash :: Text,
        message :: Text
      }
  | -- | An issue with the reverse side of a document.
    --   The error is considered resolved when the file with reverse side of the document changes
    PassportElementErrorReverseSide
      { -- | The section of the user's Telegram Passport which has the issue,i
        --  one of 'DriverLicense', 'IdentityCard'
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | -- | An issue with the selfie with a document.
    --   The error is considered resolved when the file with the selfie changes.
    PassportElementErrorSelfie
      { -- | The section of the user's Telegram Passport which has the issue,
        --   one of 'Passport', 'DriverLicense', 'IdentityCard', 'InternalPassport'
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | -- | An issue with a document scan.
    --   The error is considered resolved when the file with the document scan changes.
    PassportElementErrorFile
      { -- | The section of the user's Telegram Passport which has the issue,
        --   one of 'UtilityBill', 'BankStatement', 'RentalAgreement',
        --   'PassportRegistration', 'TemporaryRegistration'
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | -- | An issue with a list of scans.
    --   The error is considered resolved when the list of files containing the scans changes.
    PassportElementErrorFiles
      { -- | The section of the user's Telegram Passport which has the issue,
        --   one of 'UtilityBill', 'BankStatement', 'RentalAgreement',
        --   'PassportRegistration', 'TemporaryRegistration'
        _type :: EncryptedPassportElementType,
        -- | List of base64-encoded file hashes
        fileHashes :: [Text],
        message :: Text
      }
  | -- | An issue with one of the files that constitute the translation of a document.
    --   The error is considered resolved when the file changes.
    PassportElementErrorTranslationFile
      { -- | Type of element of the user's Telegram Passport which has the issue,
        --   one of 'Passport', 'DriverLicense', 'IdentityCard', 'InternalPassport',
        --   'UtilityBill', 'BankStatement', 'RentalAgreement', 'PassportRegistration',
        --   'TemporaryRegistration'
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | -- | An issue with the translated version of a document.
    --   The error is considered resolved when a file with the document translation change.
    PassportElementErrorTranslationFiles
      { -- | Type of element of the user's Telegram Passport which has the issue,
        --   one of 'Passport', 'DriverLicense', 'IdentityCard', 'InternalPassport',
        --   'UtilityBill', 'BankStatement', 'RentalAgreement', 'PassportRegistration',
        --   'TemporaryRegistration'
        _type :: EncryptedPassportElementType,
        fileHashes :: [Text],
        message :: Text
      }
  | -- | An issue in an unspecified place.
    --   The error is considered resolved when new data is added.
    PassportElementErrorUnspecified
      { -- | Type of element of the user's Telegram Passport which has the issue
        _type :: EncryptedPassportElementType,
        -- | Base64-encoded element hash
        elementHash :: Text,
        message :: Text
      }
  deriving stock (Show, Eq)

mkLabel ''PassportElementError
deriveJSON
  ( defaultOptions
      { sumEncoding = TaggedObject "source" undefined,
        omitNothingFields = True,
        fieldLabelModifier = camelTo2 '_' . tryStrip "_",
        constructorTagModifier = camelTo2 '_' . tryStrip "PassportElementError" . tryStripSuffix "Field"
      }
  )
  ''PassportElementError
