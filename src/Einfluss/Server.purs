module Einfluss.Server where

-- Custom Prelude.
import Einfluss.Prelude

-- Library imports.
import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (EXCEPTION, Error, message, throw)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Foreign (ForeignError(JSONError), MultipleErrors)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.List.NonEmpty (singleton)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Node.Express.App (App, listenHttp, post, useExternal, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Request (getBody)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (Request, Response, ExpressM)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)

-- Local imports
import Einfluss.Request (mkInfluxRows, insertInfluxRows)
import Einfluss.Types

--------------------------------------------------------------------------------
foreign import rawBodyParser ::
  ∀ e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

-- | Newtype wrapper for the errors that we can encounter when parsing and
-- decoding our request body.
newtype ReqBodyErrs
  = ReqBodyErrs
  { msg    :: String
  , errors :: MultipleErrors
  }
derive instance newtypeReqErrs :: Newtype ReqBodyErrs _

-- | Endpoint handler for decoding a JSON payload from The Things Network,
-- generating an array of rows to be inserted in InfluxDB, and collecting the
-- responses.
insertRowHandler :: ∀ eff. Ctx -> Handler (ServerEff eff)
insertRowHandler ctx = do
  foreignBody <- getBody
  let body = lmap handleForeignErrs foreignBody

  case (body >>= parseTTNPayload) of
    Left errs -> do
        setStatus 400
        sendJson <<< unwrap $ errs

    Right payload -> do
      let rows = mkInfluxRows payload
      influxResps <- liftAff $ insertInfluxRows ctx rows
      let resps = map mkResp influxResps
      sendJson resps

  where
    parseTTNPayload :: String -> Either ReqBodyErrs TTNPayload
    parseTTNPayload json =
      lmap handleJSONParseErrs $ jsonParser json >>= decodeJson

    handleForeignErrs :: MultipleErrors -> ReqBodyErrs
    handleForeignErrs errs = ReqBodyErrs $
      { msg: "Failed to parse request body."
      , errors: errs
      }

    handleJSONParseErrs :: String -> ReqBodyErrs
    handleJSONParseErrs err = ReqBodyErrs $
      { msg: "Failed to parse JSON body."
      , errors: singleton (JSONError err)
      }

    mkResp :: AffjaxResponse String -> { status :: Int, message :: String }
    mkResp r = { status: getStatusCode r.status, message: r.response }
      where getStatusCode :: StatusCode -> Int
            getStatusCode (StatusCode c) = c

--------------------------------------------------------------------------------
errorHandler :: ∀ e. Ctx -> Error -> Handler e
errorHandler _ err = do
  setStatus 400
  sendJson {error: message err}

--------------------------------------------------------------------------------
-- | Given a 'Ctx', connect the endpoint handlers up for our application.
app :: ∀ eff. Ctx -> App (ServerEff eff)
app ctx = do
  useExternal rawBodyParser
  post "/api/ttn-influx" $ insertRowHandler ctx
  useOnError             $ errorHandler     ctx

--------------------------------------------------------------------------------
data Environment = Development | Production
derive instance genericEnvironment :: Generic Environment _

envFromString :: String -> Maybe Environment
envFromString "Development" = Just Development
envFromString "Production"  = Just Production
envFromString _             = Nothing

startApp :: ∀ eff. Eff (AppEff eff) Server
startApp = do
  env' <- lookupDefault Development "ENV" envFromString
  ctx  <- case env' of
    Development ->
      pure $ Ctx { dbHost: "localhost"
                 , dbName: ""
                 , dbPort: 8086
                 , dbUser: "root"
                 , dbPass: "root"
                 }
    Production -> do
      dbHost <- lookupString "DB_HOST"
      dbName <- lookupString "DB_NAME"
      dbPort <- lookupInt    "DB_PORT"
      dbUser <- lookupString "DB_USER"
      dbPass <- lookupString "DB_PASS"
      pure $ Ctx {dbHost, dbName, dbPort, dbUser, dbPass}

  port <- lookupDefault 8080 "PORT" fromString
  listenHttp (app ctx) port $ const (EC.log $ "Listening on " <> show port)

  where
    lookupDefault :: ∀ a e. a -> String -> (String -> Maybe a) -> Eff (P e) a
    lookupDefault def var modifyVar = do
      maybeEnv <- lookupEnv var
      pure $ fromMaybe def $ maybeEnv >>= modifyVar

    lookupInt :: ∀ e. String -> Eff (EP e) Int
    lookupInt str = do
      var <- lookupString str
      maybe (throw "Couldn't parse Int from String!") pure (fromString var)

    lookupString :: ∀ e. String -> Eff (EP e) String
    lookupString str = do
      maybeVar <- lookupEnv str
      case maybeVar of
        Nothing  -> throw $ "Couldn't find '" <> str <> "' environment variable!"
        Just var -> pure var

type EP eff = (exception :: EXCEPTION, process :: PROCESS | eff)
type P eff = (process :: PROCESS | eff)
