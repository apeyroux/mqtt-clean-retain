{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO (putStrLn)
import           GHC.Generics
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.MQTT.Client as MQTTC
import           Servant
import           Servant.API
import           Servant.Client
import           System.Environment
import           System.Exit

data Configuration = Configuration {
  cfgMqttApiBaseURL :: Text
  , cfgMqttApiPort :: Int
  , cfgMqttBaseURL :: Text
  , cfgMqttPort :: Int
  , cfgMqttApiKey :: Text
  , cfgMqttLogin :: Text -- retain
  , cfgMqttPassword :: Text -- clean
} deriving (Show, Generic)

instance FromJSON Configuration
instance ToJSON Configuration

data Entrie = Entrie {
  ePayload :: Text
  , eTopic :: Text
} deriving (Show, Generic)

instance FromJSON Entrie where
  parseJSON (Object v) = Entrie <$> v .: "payload" <*> v .: "topic"

data RetainShowResponse = RetainShowResponse {
  rrType :: Text
  , rrTable :: [Entrie]
} deriving (Show, Generic)

instance FromJSON RetainShowResponse where
  parseJSON (Object v) = RetainShowResponse <$> v .: "type" <*> v .: "table"

data User = User {
  user :: Text
  , pass :: Text
} deriving (Eq, Show)

-- [alex@taf:~]$ curl -vvv http://1bzKYsbh71ovYC4mrS8uzgVMFFGOlPBn@10.227.192.255/api/v1/retain/show | jq
type MQTTAPI = BasicAuth "mqtt-api auth" User :> "api" :> "v1" :> "retain" :> "show" :> Get '[JSON] RetainShowResponse

mqttApi :: Proxy MQTTAPI
mqttApi = Proxy

mqttApiC :: BasicAuthData -> ClientM RetainShowResponse
mqttApiC = client mqttApi

-- 1bzKYsbh71ovYC4mrS8uzgVMFFGOlPBn

-- purgeRetain :: RetainShowResponse -> RetainShowResponse
purgeRetain :: [Entrie] -> ReaderT Configuration IO [Entrie]
purgeRetain [] = return []
purgeRetain topics = do
  -- purge topics
  cfg <- ask
  liftIO $ mapM_ (\t -> do
            TIO.putStrLn $ "clean topic: " <> eTopic t <> " payload: " <> ePayload t
            mc <- MQTTC.runClientTLS MQTTC.mqttConfig {
              MQTTC._hostname = (T.unpack $ cfgMqttBaseURL cfg)
              , MQTTC._port = cfgMqttPort cfg
              , MQTTC._username = (Just (T.unpack $ cfgMqttLogin cfg))
              , MQTTC._password = (Just (T.unpack $ cfgMqttPassword cfg))
              , MQTTC._connID = "mqtt-clean-retain"
              }
            MQTTC.publish mc (eTopic t) "" True
        ) topics
  -- check si la purge est bien fini
  fetchRetainTopics >>= purgeRetain

fetchRetainTopics :: ReaderT Configuration IO [Entrie]
fetchRetainTopics = do
  cfg <- ask
  manager' <- liftIO $ newManager defaultManagerSettings
  r <- liftIO $ runClientM (mqttApiC (u cfg)) (mkClientEnv manager' (BaseUrl Http (T.unpack $ cfgMqttApiBaseURL cfg) (cfgMqttApiPort cfg) ""))
  case r of
    Left e -> do
      liftIO $ print e
      return []
    Right r -> return $ rrTable r
  where
    u cfg = BasicAuthData (encodeUtf8 $ cfgMqttApiKey cfg) ""

main :: IO ()
main = do

  args <- getArgs

  case args of
    [cfgF] -> do
      cfgraw <- B.readFile cfgF
      case decode cfgraw :: Maybe Configuration of
        Just c -> do
          putStrLn "début du traitement"
          runReaderT (fetchRetainTopics >>= purgeRetain) c
          putStrLn "fin du traitement"
        Nothing -> do
          putStrLn "Je ne comprend pas le fichier de configuration."
          exitFailure
    _ -> putStrLn "usage: mqtt-clean-retain ./path/du/fichier/de/config.json"
