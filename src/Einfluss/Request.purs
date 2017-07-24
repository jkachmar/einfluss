module Einfluss.Request where

-- Custom Prelude.
import Einfluss.Prelude
import Einfluss.Types

import Data.HTTP.Method (Method(..))
import Data.MediaType (MediaType(..))
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))

--------------------------------------------------------------------------------
-- | Given a row to insert into InfluxDB, create an AJAX request to insert it
-- using the InfluxDB line protocol.
mkInfluxReq :: ∀ e a. Respondable a => Ctx -> InfluxRow -> Affjax e a
mkInfluxReq (Ctx ctx) row =
  let headers' = [ContentType <<< MediaType $ "application/json"]
      url' = "https://" <> ctx.dbHost <> ":" <> (show ctx.dbPort) <> "/write"
  in  affjax $ defaultRequest { method = Left POST
                              , url = url'
                              , headers = headers'
                              , content = Just row
                              , username = Just ctx.dbUser
                              , password = Just ctx.dbPass
                              , withCredentials = true
                              }

-- | Type synonym for an array of 'Affjax' actions.
type AffjaxArray e a =
  Aff (ajax :: AJAX | e)
      (Array (AffjaxResponse a))

-- | Given many rows to insert into InfluxDB, run the insertions in parallel and
-- collect the responses.
insertInfluxRows :: ∀ eff. Ctx -> Array InfluxRow -> AffjaxArray eff String
insertInfluxRows ctx = parTraverse $ mkInfluxReq ctx

--------------------------------------------------------------------------------
-- | Generate an array of 'InfluxRow's for a given 'TTNPayload'.
mkInfluxRows :: TTNPayload -> Array InfluxRow
mkInfluxRows (TTNPayload p) =
  let kvs  = zip (keys p.fields) (values p.fields)
  in  map (InfluxRow <<< go) kvs
  where
    go :: Tuple String Number -> String
    go (key /\ val) =
      let ts = (pow 10.0 6.0) * (unwrap <<< unInstant $ p.timestamp)
      in  key <> ",app_id=" <> p.appId
              <> ",dev_id=" <> p.devId
              <> " value="  <> (show val)
              <> " "        <> (show ts)
