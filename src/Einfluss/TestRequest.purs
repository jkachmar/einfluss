module Einfluss.TestRequest where

-- Custom Prelude.
import Einfluss.Prelude
import Einfluss.Request
import Einfluss.Types

import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Traversable (traverse_)
import Network.HTTP.StatusCode (StatusCode(..))

--------------------------------------------------------------------------------
payloadJson :: Either String Json
payloadJson = jsonParser payloadStr

ttnPayload :: Either String TTNPayload
ttnPayload = payloadJson >>= decodeJson

influxRows :: Array InfluxRow
influxRows = either (const []) mkInfluxRows ttnPayload

showInfluxRow :: Array String
showInfluxRow = map genericShow influxRows

showTTNPayload :: String
showTTNPayload = either id genericShow ttnPayload

ttnNanoseconds :: TTNPayload -> Number
ttnNanoseconds = getNanoseconds <<< _.timestamp <<< unwrap

getNanoseconds :: Instant -> Number
getNanoseconds = (*) (pow 10.0 6.0) <<< unwrap <<< unInstant

testInfluxReqs :: Eff _ Unit
testInfluxReqs = void $ launchAff $ do
  let myCtx = Ctx { dbHost: "myHost"
                  , dbName: "myName"
                  , dbPort: 8080
                  , dbUser: "me"
                  , dbPass: "password"
                  }
  resps <- insertInfluxRows myCtx influxRows
  let status = map (show <<< _.status) resps
  traverse_ log status
  traverse_ (log <<< show <<< (f <<< _.status)) resps

  where
    f sc@(StatusCode c) =
      if (c >= 300 && c < 200)
      then Left  $ "Request failed: "   <> show sc
      else Right $ "Request succeded: " <> show sc

--------------------------------------------------------------------------------
-- | Raw string representation of our TTN JSON payload, to test decoding.
payloadStr :: String
payloadStr = """{
  "app_id": "my-ttn-app",
  "dev_id": "my-device-id",
  "hardware_serial": "DEVICE_DEV_EUI",
  "port": 5,
  "counter": 0,
  "payload_raw": "AQDsAjoEABkFAAcODA==",
  "payload_fields": {
    "humidity": 55,
    "light": 25,
    "motion": 2,
    "temperature": 23.0,
    "vdd": 3.672,
    "co2": 400
  },
  "metadata": {
    "time": "2017-07-01T12:15:00.00000000Z",
    "frequency": 903.9,
    "modulation": "LORA",
    "data_rate": "SF7BW125",
    "coding_rate": "4/5",
    "gateways": [
      {
        "gtw_id": "my-ttn-gateway",
        "gtw_trusted": true,
        "timestamp": 3204665211,
        "time": "2017-07-01T12:15:00Z",
        "channel": 0,
        "rssi": -85,
        "snr": -5.5,
        "latitude": 0.00000,
        "longitude": 0.00000
      }
    ]
  },
  "downlink_url": "https://integrations.thethingsnetwork.org/ttn-us-west/api/v2/down/my-ttn-gateway/my-ttn-gateway?key=ttn-account-v2.access-key"
}"""
