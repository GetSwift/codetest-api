{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib where

import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.Aeson
import           Data.Proxy
import           Data.Text                as T (Text)
import qualified Data.Text                as T
import           Data.Text.IO             as T (readFile, writeFile)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Random

type Timestamp = Integer

type API = "drone"   :> Get '[JSON] Drone
      :<|> "package" :> Get '[JSON] Package

data Location = Location
  { latitude  :: Double
  , longitude :: Double
  } deriving (Generic, Show)
instance ToJSON Location

data Package = Package
  { packageId   :: Integer
  , destination :: Location
  , deadline    :: Timestamp
  } deriving (Generic, Show)
instance ToJSON Package

data Drone = Drone
  { droneId  :: Integer
  , location :: Location
  , packages :: [Package]
  } deriving (Generic, Show)
instance ToJSON Drone

randomLocation :: RandomGen g => Rand g Location
randomLocation = Location
  <$> getRandomR (-37.765, -37.785)
  <*> getRandomR (144.847, 144.865)

randomPackage :: RandomGen g => Timestamp -> Rand g Package
randomPackage ts = Package
  <$> getRandomR (1, 10000)
  <*> randomLocation
  <*> getRandomR (ts + 360, ts + 3600)

randomDrone :: RandomGen g => Timestamp -> Rand g Drone
randomDrone ts = Drone
  <$> getRandomR (10000, 1000000)
  <*> randomLocation
  <*> pure []

randIO :: (MonadIO m) => Rand StdGen a -> m a
randIO r = liftIO . getStdRandom $ \gen -> runRand r gen

api :: Proxy API
api = Proxy

server :: Server API
server = randIO (randomDrone 1000)
    :<|> randIO (randomPackage 1000)

app :: Application
app = serve api server

someFunc :: IO ()
someFunc = run 3000 app
