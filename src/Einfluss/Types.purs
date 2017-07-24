module Einfluss.Types
  ( AppEff
  , Ctx(..)
  , InfluxRow(..)
  , ServerEff
  , TTNPayload(..)
  ) where

-- Custom prelude.
import Einfluss.Prelude

import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.JSDate (parse, toInstant)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax.Request (class Requestable)
import Node.Express.Types (EXPRESS)
import Node.Process (PROCESS)

--------------------------------------------------------------------------------
-- | Newtype wrapper for a record of our application's context, to be read in
-- when the application starts.
newtype Ctx
  = Ctx
  { dbHost :: String
  , dbName :: String
  , dbPort :: Int
  , dbUser :: String
  , dbPass :: String
  }
derive instance newtypeCtx :: Newtype Ctx _

--------------------------------------------------------------------------------
-- | Type synonym for the effects that our server needs to perform.
type ServerEff eff =
  ( ajax      :: AJAX
  , console   :: EC.CONSOLE
  , exception :: EXCEPTION
  | eff
  )

--------------------------------------------------------------------------------
-- | Type synonym for the effects that our application needs to perform.
type AppEff eff =
  ( ajax      :: AJAX
  , console   :: EC.CONSOLE
  , exception :: EXCEPTION
  , express   :: EXPRESS
  , process   :: PROCESS
  | eff
  )

--------------------------------------------------------------------------------
-- | Representation of the relevant data extracted from a Things Network POST.
newtype TTNPayload
  = TTNPayload
  { appId     :: String
  , devId     :: String
  , timestamp :: Instant
  , fields    :: StrMap Number
  }
derive instance newtypeTTNPayload :: Newtype TTNPayload _
derive instance genericTTNPayload :: Generic TTNPayload _

--------------------------------------------------------------------------------
-- | This is actually not terribly unsafe, we're parsing in an ISO8601 string
-- representation of a date, so 'LOCALE' isn't even pertinent here.
readDate :: String -> Either String Instant
readDate =
  note "Failed to parse JSDate" <<< toInstant <<< unsafePerformEff <<< parse

instance decodeJsonTTNPayload :: DecodeJson TTNPayload where
  decodeJson json = do
    obj <- decodeJson json -- Unwrap the top level JSON object

    appId  <- obj .? "app_id"
    devId  <- obj .? "dev_id"
    fields <- obj .? "payload_fields"

    -- Parse the timestamp out from the nested 'metadata.time' field
    timestamp <- obj .? "metadata" >>= \o -> o .? "time" >>= readDate

    pure $ TTNPayload { appId, devId, timestamp, fields }

--------------------------------------------------------------------------------
-- | Newtype wrapper around the query parameter InfluxDB expects.
newtype InfluxRow = InfluxRow String
derive         instance newtypeInfluxRow     :: Newtype     InfluxRow _
derive         instance genericInfluxRow     :: Generic     InfluxRow _
derive newtype instance requestableInfluxRow :: Requestable InfluxRow
