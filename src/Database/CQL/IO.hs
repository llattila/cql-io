-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | This driver operates on some state which must be initialised prior to
-- executing client operations and terminated eventually. The library uses
-- <http://hackage.haskell.org/package/tinylog tinylog> for its logging
-- output and expects a 'Logger'.
--
-- For example:
--
-- @
-- > import Data.Text (Text)
-- > import Data.Functor.Identity
-- > import Database.CQL.IO as Client
-- > import Database.CQL.Protocol
-- > import qualified System.Logger as Logger
-- >
-- > g <- Logger.new Logger.defSettings
-- > c <- Client.init g defSettings
-- > let p = QueryParams One False () Nothing Nothing Nothing
-- > runClient c $ query ("SELECT cql_version from system.local" :: QueryString R () (Identity Text)) p
-- [Identity "3.2.0"]
-- > shutdown c
-- @
--
module Database.CQL.IO
    ( -- * Driver settings
      Settings
    , defSettings
    , addContact
    , setCompression
    , setConnectTimeout
    , setContacts
    , setIdleTimeout
    , setKeyspace
    , setMaxConnections
    , setMaxStreams
    , setMaxTimeouts
    , setMaxWaitQueue
    , setPolicy
    , setPoolStripes
    , setPortNumber
    , setProtocolVersion
    , setResponseTimeout
    , setSendTimeout

      -- * Client monad
    , Client
    , ClientState
    , DebugInfo (..)
    , init
    , runClient
    , shutdown
    , debugInfo

    , query
    , write
    , schema

    , prepare
    , prepareWrite
    , prepareSchema

    , execute
    , executeWrite
    , executeSchema

    , register
    , batch

    , request
    , command

      -- * Policies
    , Policy (..)
    , random
    , roundRobin

      -- ** Host representation
    , Host
    , HostEvent (..)
    , InetAddr  (..)

    -- * Exceptions
    , InvalidSettings    (..)
    , InternalError      (..)
    , HostError          (..)
    , ConnectionError    (..)
    , UnexpectedResponse (..)
    , Timeout            (..)
    ) where

import Control.Monad.Catch
import Control.Monad (void)
import Database.CQL.Protocol
import Database.CQL.IO.Client
import Database.CQL.IO.Cluster.Host
import Database.CQL.IO.Cluster.Policies
import Database.CQL.IO.Settings
import Database.CQL.IO.Types
import Prelude hiding (init)

------------------------------------------------------------------------------
-- query

query' :: (Tuple a, Tuple b) => QueryString k a b -> QueryParams a -> Client (Response k a b)
query' q p = do
    r <- request (RqQuery (Query q p))
    case r of
        RsError _ e -> throwM e
        _           -> return r

query :: (Tuple a, Tuple b) => QueryString R a b -> QueryParams a -> Client [b]
query q p = do
    r <- query' q p
    case r of
        RsResult _ (RowsResult _ b) -> return b
        _                           -> throwM UnexpectedResponse

write :: Tuple a => QueryString W a () -> QueryParams a -> Client ()
write q p = void $ query' q p

schema :: Tuple a => QueryString S a () -> QueryParams a -> Client (Maybe SchemaChange)
schema x y = do
    r <- query' x y
    case r of
        RsResult _ (SchemaChangeResult s) -> return $ Just s
        RsResult _ VoidResult             -> return Nothing
        _                                 -> throwM UnexpectedResponse

------------------------------------------------------------------------------
-- prepare

prepare' :: (Tuple a, Tuple b) => QueryString k a b -> Client (QueryId k a b)
prepare' q = do
    r <- request (RqPrepare (Prepare q))
    case r of
        RsResult _ (PreparedResult i _ _) -> return i
        RsError  _ e                      -> throwM e
        _                                 -> throwM UnexpectedResponse

prepare :: (Tuple a, Tuple b) => QueryString R a b -> Client (QueryId R a b)
prepare = prepare'

prepareWrite :: Tuple a => QueryString W a () -> Client (QueryId W a ())
prepareWrite = prepare'

prepareSchema :: Tuple a => QueryString S a () -> Client (QueryId S a ())
prepareSchema = prepare'

------------------------------------------------------------------------------
-- execute

execute' :: (Tuple a, Tuple b) => QueryId k a b -> QueryParams a -> Client (Response k a b)
execute' q p = do
    r <- request (RqExecute (Execute q p))
    case r of
        RsError  _ e -> throwM e
        _            -> return r

execute :: (Tuple a, Tuple b) => QueryId R a b -> QueryParams a -> Client [b]
execute q p = do
    r <- execute' q p
    case r of
        RsResult _ (RowsResult _ b) -> return b
        _                           -> throwM UnexpectedResponse

executeWrite :: Tuple a => QueryId W a () -> QueryParams a -> Client ()
executeWrite q p = void $ execute' q p

executeSchema :: Tuple a => QueryId S a () -> QueryParams a -> Client (Maybe SchemaChange)
executeSchema q p = do
    r <- execute' q p
    case r of
        RsResult _ (SchemaChangeResult s) -> return $ Just s
        RsResult _ VoidResult             -> return Nothing
        _                                 -> throwM UnexpectedResponse

batch :: Batch -> Client ()
batch b = command (RqBatch b)

------------------------------------------------------------------------------
-- register

register :: [EventType] -> Client ()
register = command . RqRegister . Register
