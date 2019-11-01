{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module ServantShim
  ( makeHandler
  ) where

import           AWSLambda.Events.APIGateway
import           Control.Exception (throwIO)
import           Control.Concurrent.MVar.Strict (MVar, newEmptyMVar, putMVar,
                                                 takeMVar)
import           Data.Aeson                  (Value, decode, encode)
import           Data.Aeson.Embedded         (Embedded)
import           Data.ByteString.Builder     (toLazyByteString)
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy        as BL
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.IO.Exception            (IOError, IOErrorType (OtherError),
                                              IOException (IOError))
import           Network.AWS.Lens            ((%~), (?~), (^.), (&))
import           Network.HTTP.Types          hiding (Header)
import           Network.Wai                 (Application,
                                              RequestBodyLength (ChunkedBody),
                                              defaultRequest)
import           Network.Wai.Internal        (Request (..), Response (..),
                                              ResponseReceived (..))


type APIGatewayHandler
   = APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded Value))

-- | Constructs a APIGatewayHandler for a request to this AWS Lambda function.
--
-- A handler converts the APIGatewayProxyRequest into a regular WAI Request
-- value. This is then passed into the Servant application and handled as if it
-- was a regular Servant request.
--
-- The passed Application is used to handle all requests coming into the handler.
makeHandler :: Application -> APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded Value))
makeHandler application apiGatewayRequest = do
  -- Convert `APIGateWayProxyRequest` to `Request`
  let reqBody :: B.ByteString
      reqBody = maybe mempty (BL.toStrict . encode) $
                  apiGatewayRequest ^. requestBodyEmbedded
      request :: Request
      request = proxyToRequest reqBody apiGatewayRequest
  -- Need an MVar because you can only return `ResponseReceived` from
  -- a wai application.
  responseMVar <- newEmptyMVar
  _ <- application request $ writeResponseTo responseMVar
  result <- takeMVar responseMVar
  pure . responseFromBody . decode $ result

-- | Streams an HTTP response into a MVar, confirming receipt of the response.
--
-- This is intended to be used to construct the second argument to the WAI
-- Application type, a (Response -> IO ResponseReceived) function. This allows
-- us to extract the response from a WAI application without sending that
-- response to the client.
writeResponseTo :: MVar BL.ByteString -> Response -> IO ResponseReceived
writeResponseTo responseRef resp = do
    case resp of
      ResponseBuilder _ _ builder -> do
        putMVar responseRef (toLazyByteString builder)
        pure ResponseReceived
      _ ->
        throwIO $
          IOError
            Nothing
            OtherError
            ""
            "unable to process response"
            Nothing
            Nothing


addCorsHeaders :: ResponseHeaders -> ResponseHeaders
addCorsHeaders headers = allowOrigin : allowCredentials : headers
  where
    allowOrigin = ("Access-Control-Allow-Origin", "*")
    allowCredentials = ("Access-Control-Allow-Credentials", "true")

-- | Convert decoded `ByteString` from `Application`'s `Response`
-- to `APIGatewayProxyResponse`.
responseFromBody :: Maybe Value -> APIGatewayProxyResponse (Embedded Value)
responseFromBody Nothing = responseBadRequest
responseFromBody (Just body) =
  responseOK & responseBodyEmbedded ?~ body & agprsHeaders %~ addCorsHeaders

proxyToRequest :: B.ByteString -> APIGatewayProxyRequest (Embedded Value) -> Request
proxyToRequest reqBody apiGWRequest =
  defaultRequest
    { requestMethod = apiGWRequest ^. agprqHttpMethod
    , httpVersion = http11
    , rawPathInfo = rawPath
    , rawQueryString = query
    , requestHeaders = apiGWRequest ^. agprqHeaders
    , pathInfo = pathParts
    , queryString = apiGWRequest ^. agprqQueryStringParameters
    , Network.Wai.Internal.requestBody = pure reqBody
    , requestBodyLength = ChunkedBody
    }
  where
    rawPath :: B.ByteString
    rawPath = apiGWRequest ^. agprqPath
    pathParts :: [T.Text]
    pathParts = drop 1 . T.splitOn "/" $ T.decodeUtf8 rawPath
    query :: B.ByteString
    query = renderQuery True $ apiGWRequest ^. agprqQueryStringParameters
