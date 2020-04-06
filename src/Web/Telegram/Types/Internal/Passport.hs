{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.Types.Internal.Passport where

import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Utils

data PassportData
  = PassportData
      { passportData :: [EncryptedPassportElement],
        credentials :: EncryptedCredentials
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "passport" PassportData
  deriving (ToHttpApiData) via Serialize PassportData

data PassportFile
  = PassportFile
      { fileId :: Text,
        fileUniqueId :: Text,
        fileSize :: Integer,
        fileDate :: Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake PassportFile
  deriving (ToHttpApiData) via Serialize PassportFile

data EncryptedPassportElement
  = EncryptedPassportElement
      { elementType :: EncryptedPassportElementType,
        elementData :: Maybe Text,
        phoneNumber :: Maybe Text,
        email :: Maybe Text,
        files :: Maybe [PassportFile],
        frontSide :: Maybe PassportFile,
        reverseSide :: Maybe PassportFile,
        selfie :: Maybe PassportFile,
        translation :: Maybe [PassportFile],
        hash :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "element" EncryptedPassportElement

data EncryptedCredentials
  = EncryptedCredentials
      { credentialData :: Text,
        hash :: Text,
        secret :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "credential" EncryptedCredentials

data PassportElementError
  = PassportElementErrorDataField
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fieldName :: Text,
        dataHash :: Text,
        message :: Text
      }
  | PassportElementErrorFrontSide
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorReverseSide
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorSelfie
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorFile
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorFiles
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHashes :: [Text],
        message :: Text
      }
  | PassportElementErrorTranslationFile
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHash :: Text,
        message :: Text
      }
  | PassportElementErrorTranslationFiles
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        fileHashes :: [Text],
        message :: Text
      }
  | PassportElementErrorUnspecified
      { source :: Text,
        errorType :: EncryptedPassportElementType,
        elementHash :: Text,
        message :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "error" PassportElementError
  deriving (ToHttpApiData) via Serialize PassportElementError

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
  deriving (Show, Eq, Ord, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier CamelToSnake] EncryptedPassportElementType
  deriving (ToHttpApiData) via Serialize EncryptedPassportElementType
