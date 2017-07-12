{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.Aeson
import           Data.List
import           Data.Proxy
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Random

type Timestamp = Integer

type API = "drone"   :> Get '[JSON] Drone
      :<|> "package" :> Get '[JSON] Package
      :<|> "drones" :> Get '[JSON] [Drone]
      :<|> "packages" :> Get '[JSON] [Package]

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
instance Ord Package where
  (Package pId1 _ _) `compare` (Package pId2 _ _) = pId1 `compare` pId2
instance Eq Package where
  (Package pId1 _ _) == (Package pId2 _ _) = pId1 == pId2

data Drone = Drone
  { droneId  :: Integer
  , location :: Location
  , packages :: [Package]
  } deriving (Generic, Show)
instance ToJSON Drone
instance Ord Drone where
  (Drone dId1 _ _) `compare` (Drone dId2 _ _) = dId1 `compare` dId2
instance Eq Drone where
  (Drone dId1 _ _) == (Drone dId2 _ _) = dId1 == dId2

randomLocation :: RandomGen g => Rand g Location
randomLocation = Location
  <$> getRandomR (-37.765, -37.785)
  <*> getRandomR (144.847, 144.865)

randomPackage :: RandomGen g => Timestamp -> Rand g Package
randomPackage ts = Package
  <$> getRandomR (1, 10000)
  <*> randomLocation
  <*> getRandomR (ts + 360, ts + 3600)

randomDrone :: RandomGen g => Rand g Drone
randomDrone = Drone
  <$> getRandomR (10000, 1000000)
  <*> randomLocation
  <*> pure []

randomList :: (Ord a, RandomGen g) => Int -> Rand g a -> Rand g [a]
randomList n r = do
  n <- getRandomR (0, n)
  withoutDupes <$> sequence (replicate n r)
  where withoutDupes = map head . group . sort

randIO :: (MonadIO m) => Rand StdGen a -> m a
randIO r = liftIO . getStdRandom $ \gen -> runRand r gen

api :: Proxy API
api = Proxy

server :: Server API
server = randIO randomDrone
    :<|> randIO (randomPackage 1500000000)
    :<|> randIO (randomList 100 randomDrone)
    :<|> do
      ts <- liftIO $ round <$> getPOSIXTime
      randIO (randomList 100 $ randomPackage ts)
  where replicRand n r = randIO $ sequence (replicate n r)

app :: Application
app = serve api server

runApi :: IO ()
runApi = run 3000 app
