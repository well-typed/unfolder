-- | Our starting point: an untyped protocol
module Untyped where

import Data.Time
import Data.Aeson

import Util.Aeson

data Request =
    GetTime
  | GetFile FilePath

data Response =
    Time UTCTime
  | File String

-- | Poor type design
--
-- There is no link between the request and the type of the response.
sendRequest :: Request -> IO Response
sendRequest = undefined

-- | Poor protocol design
--
-- The problem isn't just at the type-level: it's on the wire! When we encode
-- the response, we must include a tag what kind of response it is, so whether
-- it's at the type-level or not, clients need to deal with cases that they
-- should really not have to deal with. The only response possible to 'GetTime'
-- is a 'UTCTime', and the only response to 'GetFile' is a 'String'; there
-- should be no need for a tag.
instance ToJSON Response where
  toJSON (Time time) = object [key "tag" .= "Time", key "payload" .= time]
  toJSON (File file) = object [key "tag" .= "File", key "payload" .= file]

