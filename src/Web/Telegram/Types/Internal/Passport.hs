{-# LANGUAGE DeriveAnyClass #-}

module Web.Telegram.Types.Internal.Passport where

import Common
import Data.Time.Clock.POSIX

data PassportData = PassportData
  { _data :: [EncryptedPassportElement],
    credentials :: EncryptedCredentials
  }
  deriving stock (Show, Eq)

data PassportFile = PassportFile
  { fileId :: Text,
    fileUniqueId :: Text,
    fileSize :: Int,
    fileDate :: POSIXTime
  }
  deriving stock (Show, Eq)

data EncryptedPassportElement = EncryptedPassportElement
  { _type :: EncryptedPassportElementType,
    _data :: Maybe Text,
    phoneNumber :: Maybe Text,
    email :: Maybe Text,
    files :: Maybe [PassportFile],
    frontSide :: Maybe PassportFile,
    reverseSide :: Maybe PassportFile,
    selfie :: Maybe PassportFile,
    translation :: Maybe [PassportFile],
    hash :: Text
  }
  deriving stock (Show, Eq)

data EncryptedCredentials = EncryptedCredentials
  { _data :: Text,
    hash :: Text,
    secret :: Text
  }
  deriving stock (Show, Eq)

data PassportElementError
  = PassportElementErrorDataField
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fieldName :: Text,
        dataHash :: Text,
        message :: Text
      }
  | PassportElementErrorFrontSide
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorReverseSide
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorSelfie
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorFile
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorFiles
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHashes :: [Text],
        message :: Text
      }
  | PassportElementErrorTranslationFile
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorTranslationFiles
      { source :: Text,
        _type :: EncryptedPassportElementType,
        fileHashes :: [Text],
        message :: Text
      }
  | PassportElementErrorUnspecified
      { source :: Text,
        _type :: EncryptedPassportElementType,
        elementHash :: Text,
        message :: Text
      }
  deriving stock (Show, Eq)

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

mkLabel ''PassportFile
mkLabel ''EncryptedPassportElement
mkLabel ''PassportData
mkLabel ''EncryptedCredentials
mkLabel ''PassportElementError
makePrismLabels ''EncryptedPassportElementType
deriveJSON snake ''PassportFile
deriveJSON snake ''EncryptedPassportElement
deriveJSON snake ''PassportData
deriveJSON snake ''EncryptedCredentials
deriveJSON snake ''PassportElementError
deriveJSON sumSnake ''EncryptedPassportElementType
