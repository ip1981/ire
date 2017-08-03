{-# LANGUAGE OverloadedStrings #-}

module IRE.Config
  ( ConfigFile(..)
  , YOLO(..)
  , defaultConfig
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON)
import Data.Yaml (Value(Object), (.!=), (.:), (.:?))

data ConfigFile = ConfigFile
  { cfPort :: Int
  , cfSocket :: Maybe FilePath
  , cfYOLO :: YOLO
  } deriving (Show)

data YOLO = YOLO
  { yoloCfg :: FilePath
  , yoloWeights :: FilePath
  , yoloNames :: FilePath
  } deriving (Show)

defaultConfig :: ConfigFile
defaultConfig =
  ConfigFile
  { cfPort = 8080
  , cfSocket = Nothing
  , cfYOLO =
      YOLO
      { yoloCfg = "yolo.cfg"
      , yoloWeights = "yolo.weights"
      , yoloNames = "yolo.names"
      }
  }

instance FromJSON ConfigFile where
  parseJSON (Object m) =
    ConfigFile <$> m .:? "port" .!= cfPort defaultConfig <*>
    m .:? "socket" .!= cfSocket defaultConfig <*>
    m .:? "yolo" .!= cfYOLO defaultConfig
  parseJSON _ = empty

instance FromJSON YOLO where
  parseJSON (Object m) = YOLO <$> m .: "cfg" <*> m .: "weights" <*> m .: "names"
  parseJSON _ = empty
