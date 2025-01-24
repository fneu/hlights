module Dirigera.Devices where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    attributes :: Attributes,
    deviceSet :: [DeviceSet]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Device

data DeviceSet = DeviceSet
  { id :: Text,
    name :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeviceSet
