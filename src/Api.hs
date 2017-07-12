{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.Aeson
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Random

type Timestamp = Integer

-- | Servant API type.
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

-- | Generates a random location within Melbourne.
randomLocation :: RandomGen g => Rand g Location
randomLocation = Location
  <$> getRandomR (-37.765, -37.785)
  <*> getRandomR (144.847, 144.865)

-- | Generates a package with a random id, location, and deadline.
randomPackage :: RandomGen g => Timestamp -> Rand g Package
randomPackage ts = Package
  <$> getRandomR (1, 10000)
  <*> randomLocation
  <*> getRandomR (ts + 360, ts + 3600)

-- | Generates a drone with a random id and location.
randomDrone :: RandomGen g => Rand g Drone
randomDrone = Drone
  <$> getRandomR (10000, 1000000)
  <*> randomLocation
  <*> pure []

-- | Creates a randomly-sized list (up to length n) of randomly generated elements.
randomList :: (Ord a, RandomGen g) => Int -> Rand g a -> Rand g [a]
randomList n r = do
  n <- getRandomR (0, n)
  withoutDupes <$> sequence (replicate n r)
  where withoutDupes = map head . group . sort

assign :: Package -> Drone -> Drone
assign package drone = drone { packages = [package]}

assignMany :: [Package] -> [Drone] -> [Drone]
assignMany ps ds = sort $ dA <> dRest
  where n = min (length ps) (length ds)
        (pn, _) = splitAt n ps
        (dn, dRest) = splitAt n ds
        dA = zipWith assign pn dn

randomAssignedDrones :: RandomGen g => Timestamp -> Rand g [Drone]
randomAssignedDrones ts = do
  ds <- randomList 100 randomDrone
  ps <- randomList (length ds) (randomPackage ts)
  return $ assignMany ps ds

-- | Lifts `Rand g a` into an IO monad.
randIO :: MonadIO m => Rand StdGen a -> m a
randIO r = liftIO . getStdRandom $ \gen -> runRand r gen

api :: Proxy API
api = Proxy

-- | Implementation of the server type.
server :: Server API
server = randIO randomDrone
    :<|> randIO (randomPackage 1500000000)
    :<|> do
      ts <- unixTime
      randIO $ randomAssignedDrones ts
    :<|> do
      ts <- unixTime
      randIO (randomList 100 $ randomPackage ts)
  where replicRand n r = randIO $ sequence (replicate n r)
        unixTime = liftIO $ round <$> getPOSIXTime

app :: Application
app = serve api server

runApi :: IO ()
runApi = run 3000 app
