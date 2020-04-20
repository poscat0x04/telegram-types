{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.Telegram.Types.Internal.InputFile where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding
import GHC.Generics
import GHC.TypeLits
import Network.Mime
import Servant.Multipart
import System.FilePath
import Web.Telegram.Types.Internal.Utils

data InputFile
  = InputFile
      { fileName :: Text,
        mimeType :: Text,
        content :: ByteString
      }
  deriving (Show, Eq, Generic, Default, Hashable)

readInput :: FilePath -> IO InputFile
readInput fp = do
  content <- LBS.readFile fp
  let fileName = pack $ takeFileName fp
  let mimeType = decodeUtf8 $ defaultMimeLookup fileName
  return InputFile {..}

type Multi = ToMultipart Mem

newtype InputF n = InputF InputFile

instance (KnownSymbol s) => ToMultipart Mem (InputF s) where
  toMultipart (InputF f) =
    MultipartData
      { inputs = [],
        files =
          pure $
            FileData
              { fdInputName = pack $ symbolVal @s Proxy,
                fdFileName = fileName f,
                fdFileCType = mimeType f,
                fdPayload = content f
              }
      }

newtype Cert = Cert InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "certificate"

newtype Thumb = Thumb InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "thumb"

newtype Photo = Photo InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "photo"

newtype Audio = Audio InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "audio"

newtype Doc = Doc InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "document"

newtype Video = Video InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "video"

newtype Animation = Animation InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "animation"

newtype Voice = Voice InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "voice"

newtype VideoNote = VideoNote InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "video_note"

newtype Sticker = Sticker InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "sticker"

newtype PngSticker = PngSticker InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "png_sticker"

newtype TgsSticker = TgsSticker InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "tgs_sticker"

newtype Media = Media InputFile
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (Multi) via InputF "media"
