module Utils where

import Common
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS

req :: (ToParts a, FromJSON b) => String -> a -> IO b
req url p = do
  manager <- newTlsManager
  let initReq = parseRequest_ url
      parts = toParts p
  request <- formDataBody parts initReq
  resp <- httpLbs request manager
  maybe undefined pure (decode (responseBody resp))
