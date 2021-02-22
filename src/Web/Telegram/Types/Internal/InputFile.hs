module Web.Telegram.Types.Internal.InputFile where

import Common
import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (pack)
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.MultipartFormData

data InputFileType
  = Attach
  | Normal
  deriving stock (Show, Eq)

-- | The contents of a file to be uploaded
data InputFile (a :: InputFileType)
  = -- | A file stored on the Telegram servers
    FileId Text
  | -- | A HTTP URL to the file
    Url Text
  | -- | A file represented as a stream of 'ByteString's
    Stream
      { name :: FilePath,
        stream :: ConduitT () BS.ByteString IO ()
      }
  | -- | A file in memory
    InMemory
      { name :: FilePath,
        file :: ByteString
      }
  deriving stock (Generic)

instance ToJSON (InputFile 'Attach) where
  toJSON (FileId t) = toJSON t
  toJSON (Url t) = toJSON t
  toJSON (Stream n _) = toJSON ("attach://" <> n)
  toJSON (InMemory n _) = toJSON ("attach://" <> n)

instance Show (InputFile a) where
  show (FileId t) = "FileId " <> show t
  show (Url t) = "Url " <> show t
  show (Stream n _) = "Stream " <> n
  show (InMemory n _) = "InMemory " <> n

instance Eq (InputFile a) where
  (FileId t1) == (FileId t2) = t1 == t2
  (Url t1) == (Url t2) = t1 == t2
  (InMemory n1 f1) == (InMemory n2 f2) = n1 == n2 && f1 == f2
  _ == _ = False

instance ToReqBody (InputFile 'Normal) where
  toReqBody (FileId t) = toReqBody t
  toReqBody (Url t) = toReqBody t
  toReqBody (Stream n s) = (Just n, requestBodySourceChunked s)
  toReqBody (InMemory n f) = (Just n, RequestBodyBS f)

instance CollectParts (InputFile 'Attach) where
  collect (Stream n s) = pure $ partFileRequestBody (pack n) n (requestBodySourceChunked s)
  collect (InMemory n f) = pure $ partFileRequestBody (pack n) n (RequestBodyBS f)
  collect _ = mempty

mkLabel ''InputFile
makePrismLabels ''InputFile
