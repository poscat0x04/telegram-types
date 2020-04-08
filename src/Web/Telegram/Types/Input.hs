-- | Sending files
module Web.Telegram.Types.Input
  ( -- * Sending Media
    InputMediaAnimation (..),
    InputMediaAudio (..),
    InputMediaDocument (..),
    InputMediaVideo (..),
    InputMediaPhoto (..),
    InputMedia,

    -- * Uploading Files
    InputFile (..),
    VideoOrPhoto,
    Cert (..),
    Thumb (..),
    Photo (..),
    Doc (..),
    Animation (..),
    Audio (..),
    Voice (..),
    Video (..),
    VideoNote (..),
    Sticker (..),
    PngSticker (..),
    TgsSticker (..),
    Media (..),
    -- Functions
    readInput,
  )
where

import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
