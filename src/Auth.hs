module Auth (generateCodeVerifier, requestAuthCode, requestAccessToken, noSSLVerifyManager) where

import Control.Exception (try)
import Control.Monad (replicateM)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit (Manager, mkManagerSettings, newManager)
import Network.HTTP.Simple
import Network.HostName (getHostName)
import System.Random (randomRIO)

newtype CodeResponse = CodeResponse
  { code :: Text
  }
  deriving (Show, Generic)

instance FromJSON CodeResponse

newtype TokenResponse = TokenResponse
  { access_token :: Text
  }
  deriving (Show, Generic)

instance FromJSON TokenResponse

generateCodeVerifier :: IO Text
generateCodeVerifier = pack <$> replicateM 128 (randomChoice chars)
  where
    chars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']
    randomChoice xs = (xs !!) <$> randomRIO (0, length xs - 1)

createCodeChallenge :: Text -> BS.ByteString
createCodeChallenge codeVerifier =
  let hash = hashWith SHA256 (encodeUtf8 codeVerifier)
   in convertToBase Base64URLUnpadded hash

noSSLVerifyManager :: IO Manager
noSSLVerifyManager =
  let tlsSettings =
        TLSSettingsSimple
          { settingDisableCertificateValidation = True,
            settingDisableSession = False,
            settingUseServerName = True
          }
   in newManager $ mkManagerSettings tlsSettings Nothing

requestAuthCode :: Text -> Text -> IO (Maybe Text)
requestAuthCode ip codeVerifier = do
  manager <- noSSLVerifyManager
  request' <- parseRequest ("GET https://" ++ unpack ip ++ ":8443/v1/oauth/authorize")
  let request =
        setRequestManager manager $
          setRequestQueryString
            [ ("audience", Just "homesmart.local"),
              ("response_type", Just "code"),
              ("code_challenge", Just (createCodeChallenge codeVerifier)),
              ("code_challenge_method", Just "S256")
            ]
            request'

  responseResult <- try (httpLBS request) :: IO (Either HttpException (Response LBS.ByteString))
  case responseResult of
    Left ex -> do
      putStrLn $ "Failed to obtain authorization code: " ++ show ex
      return Nothing
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body :: Either String CodeResponse of
        Left err -> do
          putStrLn $ "Error parsing JSON: " ++ err
          return Nothing
        Right codeResponse -> return $ Just codeResponse.code

requestAccessToken :: Text -> Text -> Text -> IO (Maybe Text)
requestAccessToken ip codeVerifier authCode = do
  manager <- noSSLVerifyManager
  hostname <- getHostName
  request' <- parseRequest ("POST https://" ++ unpack ip ++ ":8443/v1/oauth/token")
  let request =
        setRequestManager manager
          $ setRequestHeaders
            [("Content-Type", "application/x-www-form-urlencoded")]
          $ setRequestMethod "POST"
          $ setRequestBodyURLEncoded
            [ ("code", encodeUtf8 authCode),
              ("name", encodeUtf8 $ pack hostname),
              ("grant_type", "authorization_code"),
              ("code_verifier", encodeUtf8 codeVerifier)
            ]
            request'

  responseResult <- try (httpLBS request) :: IO (Either HttpException (Response LBS.ByteString))
  case responseResult of
    Left ex -> do
      putStrLn $ "Failed to obtain access token: " ++ show ex
      return Nothing
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body :: Either String TokenResponse of
        Left err -> do
          putStrLn $ "Error parsing JSON: " ++ show body ++ "\n" ++ err
          return Nothing
        Right tokenResponse -> return $ Just tokenResponse.access_token
