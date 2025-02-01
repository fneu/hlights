module Dirigera.Devices where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

forceAttributes :: Maybe Attributes -> Attributes
forceAttributes Nothing = Attributes Nothing Nothing Nothing Nothing Nothing
forceAttributes (Just a) = a

data Attributes = Attributes
  { isOn :: Maybe Bool,
    lightLevel :: Maybe Int,
    colorTemperature :: Maybe Int,
    colorTemperatureMin :: Maybe Int,
    colorTemperatureMax :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Attributes

data Device = Device
  { id :: Text,
    deviceType :: Text,
    isReachable :: Bool,
    attributes :: Maybe Attributes,
    deviceSet :: Maybe [DeviceSet]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Device

data DeviceSet = DeviceSet
  { id :: Text,
    name :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeviceSet

data MsgWithDeviceData = MsgWithDeviceData
  { id :: Text,
    time :: Text,
    msgType :: Text,
    deviceData :: Device
  }
  deriving (Show, Eq, Generic)

instance FromJSON MsgWithDeviceData where
  parseJSON = withObject "MsgWithDeviceData" $ \o -> do
    id' <- o .: "id"
    time <- o .: "time"
    msgType <- o .: "type"
    deviceData <- o .: "data"
    pure $ MsgWithDeviceData id' time msgType deviceData

mergeAttributes :: Maybe Attributes -> Maybe Attributes -> Maybe Attributes
mergeAttributes Nothing newA = newA
mergeAttributes oldA Nothing = oldA
mergeAttributes (Just oldA) (Just newA) =
  Just $
    Attributes
      { isOn = newA.isOn <|> oldA.isOn,
        lightLevel = newA.lightLevel <|> oldA.lightLevel,
        colorTemperature = newA.colorTemperature <|> oldA.colorTemperature,
        colorTemperatureMin = newA.colorTemperatureMin <|> oldA.colorTemperatureMin,
        colorTemperatureMax = newA.colorTemperatureMax <|> oldA.colorTemperatureMax
      }

mergeDevice :: Device -> Device -> Device
mergeDevice oldD newD =
  Device
    { id = oldD.id,
      deviceType = newD.deviceType,
      isReachable = newD.isReachable,
      attributes = mergeAttributes oldD.attributes newD.attributes,
      deviceSet = newD.deviceSet <|> oldD.deviceSet
    }
