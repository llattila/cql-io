-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.CQL.IO.Cluster.Discovery where

import Data.Functor.Identity (Identity)
import Data.IP
import Data.Text (Text)
import Database.CQL.Protocol
import qualified Data.Set
import Data.Int

data Peer = Peer
    { peerAddr :: !IP
    , peerRPC  :: !IP
        -- ^ The address for the client to connect to.
    , peerDC   :: !Text
    , peerRack :: !Text
    , peerTokens :: Data.Set.Set Int
    } deriving Show

recordInstance ''Peer

peers :: QueryString R () (IP, IP, Text, Text, Set Text)
peers = "SELECT peer, rpc_address, data_center, rack, tokens FROM system.peers"

peer :: QueryString R (Identity IP) (IP, IP, Text, Text, Set Text)
peer = "SELECT peer, rpc_address, data_center, rack, tokens FROM system.peers where peer = ?"

local :: QueryString R () (Text, Text, Set Text)
local = "SELECT data_center, rack, tokens FROM system.local WHERE key='local'"
