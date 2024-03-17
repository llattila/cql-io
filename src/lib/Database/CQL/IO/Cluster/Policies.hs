-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

module Database.CQL.IO.Cluster.Policies
    ( Policy (..)
    , random
    , roundRobin
    , tokenAware
    , PolicyType (..)
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens ((^.), view, over, makeLenses)
import Control.Monad
import Data.Map.Strict (Map)
import Data.Word
import Database.CQL.IO.Cluster.Host
import System.Random.MWC
import Prelude
import Database.CQL.Protocol hiding (Map, Set)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int (Int64)

-- | A policy defines a load-balancing strategy and generally
-- handles host visibility.
data Policy = Policy
    { setup :: [Host] -> [Host] -> IO ()
      -- ^ Initialise the policy with two sets of hosts. The first
      -- parameter are hosts known to be available, the second are other
      -- nodes. Note that a policy may be re-initialised at any point
      -- through this function.
    , onEvent :: HostEvent -> IO ()
      -- ^ Event handler. Policies will be informed about cluster changes
      -- through this function.
    , select :: Maybe RoutingToken -> IO (Maybe Host)
      -- ^ Host selection. The driver will ask for a host to use in a query
      -- through this function. A policy which has no available nodes may
      -- return Nothing.
    , current :: IO [Host]
      -- ^ Return all currently alive hosts.
    , acceptable :: Host -> IO Bool
      -- ^ During startup and node discovery, the driver will ask the
      -- policy if a dicovered host should be ignored.
    , hostCount :: IO Word
      -- ^ During query processing, the driver will ask the policy for
      -- a rough estimate of alive hosts. The number is used to repeatedly
      -- invoke 'select' (with the underlying assumption that the policy
      -- returns mostly different hosts).
    , display :: IO String
      -- ^ Like having an effectful 'Show' instance for this policy.
    , policyType :: PolicyType
      -- ^ Returns the used policy type, needed for token aware routing
    }

data PolicyType = TokenAware | RoundRobin | Random
   deriving Show

type HostMap = TVar Hosts

data Hosts = Hosts
    { _alive :: !(Map InetAddr Host)
    , _other :: !(Map InetAddr Host)
    } deriving Show

makeLenses ''Hosts

-- | Iterate over hosts one by one.
roundRobin :: IO Policy
roundRobin = do
    h <- newTVarIO emptyHosts
    c <- newTVarIO 0
    return $ Policy (defSetup h) (defOnEvent h) (pickHost h c)
                    (defCurrent h) defAcceptable (defHostCount h)
                    (defDisplay h) RoundRobin
  where
    pickHost h c _ = atomically $ do
        m <- view alive <$> readTVar h
        if Map.null m then
            return Nothing
        else do
            k <- readTVar c
            writeTVar c $ succ k `mod` Map.size m
            return . Just . snd $ Map.elemAt (k `mod` Map.size m) m

-- | Return hosts in random order.
random :: IO Policy
random = do
    h <- newTVarIO emptyHosts
    g <- createSystemRandom
    return $ Policy (defSetup h) (defOnEvent h) (const (pickRandomHost h g))
                    (defCurrent h) defAcceptable (defHostCount h)
                    (defDisplay h) Random

pickRandomHost :: TVar Hosts -> GenIO -> IO (Maybe Host)
pickRandomHost h g = do
    m <- view alive <$> readTVarIO h
    if Map.null m then
        return Nothing
    else do
        let i = uniformR (0, Map.size m - 1) g
        Just . snd . flip Map.elemAt m <$> i

-- | Return host based on routing key
tokenAware :: IO Policy
tokenAware = do
    h <- newTVarIO emptyHosts
    g <- createSystemRandom
    return $ Policy (defSetup h) (defOnEvent h) (pickHost h g)
                    (defCurrent h) defAcceptable (defHostCount h)
                    (defDisplay h) TokenAware
  where
    pickHost hosts randomGen routingToken = 
      case routingToken of
        Nothing -> pickRandomHost hosts randomGen 
        Just tokenToUse -> do
          fromToken <- getHostFromRoutingToken hosts tokenToUse 
          case fromToken of
            Just host -> pure $ Just host
            Nothing -> pickRandomHost hosts randomGen 

-----------------------------------------------------------------------------
-- Defaults

emptyHosts :: Hosts
emptyHosts = Hosts Map.empty Map.empty

defDisplay :: HostMap -> IO String
defDisplay h = show <$> readTVarIO h

defAcceptable :: Host -> IO Bool
defAcceptable = const $ return True

defSetup :: HostMap -> [Host] -> [Host] -> IO ()
defSetup r a b = do
    let ha = Map.fromList $ zip (map (view hostAddr) a) a
    let hb = Map.fromList $ zip (map (view hostAddr) b) b
    let hosts = Hosts ha hb
    atomically $ writeTVar r hosts

defHostCount :: HostMap -> IO Word
defHostCount r = fromIntegral . Map.size . view alive <$> readTVarIO r

defCurrent :: HostMap -> IO [Host]
defCurrent r = Map.elems . view alive <$> readTVarIO r

defOnEvent :: HostMap -> HostEvent -> IO ()
defOnEvent r (HostNew h) = atomically $ do
    m <- readTVar r
    when (Nothing == get (h^.hostAddr) m) $
        writeTVar r (over alive (Map.insert (h^.hostAddr) h) m)
defOnEvent r (HostGone a) = atomically $ do
    m <- readTVar r
    if Map.member a (m^.alive) then
        writeTVar r (over alive (Map.delete a) m)
    else
        writeTVar r (over other (Map.delete a) m)
defOnEvent r (HostUp a) = atomically $ do
    m <- readTVar r
    case get a m of
        Nothing -> return ()
        Just  h -> writeTVar r
            $ over alive (Map.insert a h)
            . over other (Map.delete a)
            $ m
defOnEvent r (HostDown a) = atomically $ do
    m <- readTVar r
    case get a m of
        Nothing -> return ()
        Just  h -> writeTVar r
            $ over other (Map.insert a h)
            . over alive (Map.delete a)
            $ m

get :: InetAddr -> Hosts -> Maybe Host
get a m = Map.lookup a (m^.alive) <|> Map.lookup a (m^.other)

-- | Gets the correct host based on routing token. If the token is below
-- the smallest key in the hosts, then the largest key will be used

getHostFromRoutingToken :: HostMap -> RoutingToken -> IO (Maybe Host)
getHostFromRoutingToken hosts routingToken = do
     m <- view alive <$> readTVarIO hosts
     if Map.null m 
       then return Nothing
       else pure $ getMatchingHost (Map.fromList (map (\host -> (host,_tokens host)) (Map.elems m))) routingToken 
         
getMatchingHost :: Map.Map Host (Set Int64) -> RoutingToken -> Maybe Host 
getMatchingHost hostMap tokenToSeek =
  let distancesToHosts = Map.map ((\x -> getDistance x tokenToSeek) . Set.toList) hostMap
  in case fst <$> Map.lookupMin distancesToHosts of
       Nothing -> fst <$> Map.lookupMax ( Map.map (Set.lookupMax) hostMap)
       Just h -> Just h

getDistance :: [Int64] -> RoutingToken -> Maybe Int64
getDistance [] _ = Nothing
getDistance [x] (RoutingToken token) = if token > x then Just (token - x) else Nothing 
getDistance (x:y:xs) (RoutingToken token) = if token < y && token > x
  then Just $ token - x
  else getDistance (y:xs) (RoutingToken token)

