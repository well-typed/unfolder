-- | Haskell Unfolder episode 51: typed servers using sop-core
--
-- In this episode we consider how we can design a typed client/server interface
-- for a simple API. We will use this as an introduction to some of the
-- foundational concepts of the @sop-core@ library, and we will discuss some
-- ways of dealing with serialization in a more strongly typed setting. While
-- does this require a bit of type level programming, hopefully by the end of
-- this episode that won't look quite so intimidating anymore, and the benefit
-- is not just better Haskell types, but also a better bits-on-the-wire
-- communication protocol.
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.SOP (SListI, NP(..), NS(..), I(..), K(..), unK, mapIK)
import Data.SOP.NP (map_NP, sequence_NP, collapse_NP, zipWith_NP)
import Data.SOP.NP qualified as NP
import Data.Time

import Util.Aeson
import Util.Transport

{-------------------------------------------------------------------------------
  API definition and implementation

  (We will get back to the serialization issue in the second half.)
-------------------------------------------------------------------------------}

data Request a where
  GetTime :: Request UTCTime
  GetFile :: FilePath -> Request String

execRequest :: Request a -> IO a
execRequest GetTime      = getCurrentTime
execRequest (GetFile fp) = readFile fp

{-------------------------------------------------------------------------------
  Extend to multiple requests, option 1: return all results

  An 'NP' is an n-ary product:

  > data NP f as where
  >   Nil  :: NP f []
  >   (:*) :: f x -> NP f xs -> NP f (x : xs)

  For example, @NP Maybe '[Int, Bool, Char] is isomorphic to

  > (Maybe Int, Maybe Bool, Maybe Char)

  The corresponding 'map' function is:

  > map_NP      :: SListI xs => (forall a. f a -> g a) -> NP f xs -> NP g xs
  > sequence_NP :: (SListI xs, Applicative f) => NP f xs -> f (NP I xs)

  compare to

  > map       :: (a -> b) -> [a] -> [b]
  > sequenceA :: Applicative f => [f a] -> f [a]

  (There are also more general versions of these functions, called @hmap@ and
  co, but we avoid them in this episode as their types are more difficult.)
-------------------------------------------------------------------------------}

getTwoResults :: (Request a, Request b) -> IO (a, b)
getTwoResults (a, b) = (,) <$> execRequest a <*> execRequest b

getAllResults :: SListI as => NP Request as -> IO (NP I as)
getAllResults = sequence_NP . map_NP execRequest

{-------------------------------------------------------------------------------
  Extend to multiple requests, option 2: return first result only

  Compare to

  > race :: IO a -> IO b -> IO (Either a b)

  Here we need an n-ary sum:

  > data NS f as where
  >   Z :: f x     -> NS f (x : xs)
  >   S :: NS f xs -> NS f (x : xs)

  We return 'Nothing' on timeout (or if the list is empty).
-------------------------------------------------------------------------------}

getFirstOfTwo :: (Request a, Request b) -> IO (Either a b)
getFirstOfTwo (a, b) = race (execRequest a) (execRequest b)

raceN :: NP IO as -> IO (Maybe (NS I as))
raceN Nil       = Nothing <$ threadDelay 1_000_000
raceN (a :* as) = either (Just . Z . I) (fmap S) <$> race a (raceN as)

getFirstResult :: SListI as => NP Request as -> IO (Maybe (NS I as))
getFirstResult = raceN . map_NP execRequest

{-------------------------------------------------------------------------------
  JSON encoding of requests

  The 'ToJSON' for 'Request' is entirely standard, even though it's a GADT.
  For @NP Request@, we see a common pattern, using

  > newtype K b a = K { unK :: b }
  > collapse_NP :: NP (K a) xs -> [a]  -- NOTE: NP (K b) as ~ [b]
-------------------------------------------------------------------------------}

instance ToJSON (Request a) where
  toJSON GetTime      = object [key "tag" .= "GetTime"]
  toJSON (GetFile fp) = object [key "tag" .= "GetFile", key "fp" .= fp]

instance SListI as => ToJSON (NP Request as) where
  toJSON = valueList . collapse_NP . map_NP (K . toJSON)

{-------------------------------------------------------------------------------
  JSON /decoding/ of requests

  This is trickier, because we don't know what type of request to expect.
-------------------------------------------------------------------------------}

data SomeRequest where
  SomeRequest :: Request a -> SomeRequest

data SomeRequests where
  SomeRequests :: SListI as => NP Request as -> SomeRequests

instance FromJSON SomeRequest where
  parseJSON v = decodeObjectWith "Request" v $ \obj -> do
    tag <- obj .: key "tag"
    case tag of
      "GetTime"  -> return $ SomeRequest $ GetTime
      "GetFile"  -> do fp <- obj .: key "fp"
                       return $ SomeRequest $ GetFile fp
      _otherwise -> fail $ "Invalid tag " ++ show tag

instance FromJSON SomeRequests where
  parseJSON = \v -> do
      vs <- parseJSON v
      aux vs
    where
      aux :: [Value] -> Parser SomeRequests
      aux []     = return $ SomeRequests Nil
      aux (v:vs) = do
          SomeRequest  r  <- parseJSON v
          SomeRequests rs <- aux vs
          return $ SomeRequests (r :* rs)

{-------------------------------------------------------------------------------
  JSON encoding and decode of results is easier: we always know what to expect

  We use @I@ here for the identity functor.

  We use the requests as a guide to know how to encode/decode the response.

  Other approaches are possible also; for example, 'decodeRequests' could
  already anticipate the need for encoding the results, and use this signature
  instead:

  > decodeRequests ::
  >      [Value]
  >   -> (forall as. All ToJSON as => NP Request as -> Parser r)
  >   -> Parser r

  Alternatively, we could prove that all responses have 'FromJSON' instances:

  > canDecodeResults :: SListI as => NP Request as -> NP (Dict FromJSON) as
  > canDecodeResults = map_NP aux
  >   where
  >     aux :: Request a -> Dict FromJSON a
  >     aux GetTime{} = Dict
  >     aux GetFile{} = Dict
-------------------------------------------------------------------------------}

encodeResult :: SListI as => NP Request as -> NP I as -> Value
encodeResult reqs resps =
    valueList $ collapse_NP $ zipWith_NP aux reqs resps
  where
    aux :: Request a -> I a -> K Value a
    aux GetTime{} = mapIK toJSON
    aux GetFile{} = mapIK toJSON

decodeResult :: SListI as => NP Request as -> [Value] -> Parser (NP I as)
decodeResult reqs resps =
    case NP.fromList resps of
      Nothing     -> parseFail "Unexpected number of responses"
      Just resps' -> sequence_NP $ zipWith_NP aux reqs resps'
  where
    aux :: Request a -> K Value a -> Parser a
    aux GetTime{} = parseJSON . unK
    aux GetFile{} = parseJSON . unK

{-------------------------------------------------------------------------------
  Putting it all together
-------------------------------------------------------------------------------}

server :: Transport -> IO ()
server transport = forever $ do
    SomeRequests reqs <- recvValue transport
    sendValueWith transport (encodeResult reqs) =<< getAllResults reqs

client :: SListI as => Transport -> NP Request as -> IO (NP I as)
client transport reqs = do
    sendValue transport reqs
    recvValueWith transport (decodeResult reqs)

main :: IO ()
main = mockTransport server $ \transport ->
    print =<< client transport (GetTime :* GetFile "./hi.txt" :* Nil)
