-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.CQL.IO.Timeouts
    ( TimeoutManager
    , Milliseconds (..)
    , create
    , destroy
    , Action
    , add
    , cancel
    , withTimeout
    ) where

import Control.Concurrent.STM
import Control.Exception (mask_, bracket)
import Control.Reaper
import Control.Monad
import Database.CQL.IO.Exception (ignore)
import Database.CQL.IO.Connection.Settings (Milliseconds (..))

data TimeoutManager = TimeoutManager
    { roundtrip :: !Int
    , reaper    :: !(Reaper [Action] Action)
    }

data Action = Action
    { action :: !(IO ())
    , state  :: !(TVar State)
    }

data State = Running !Int | Canceled

create :: Milliseconds -> IO TimeoutManager
create (Ms n) = TimeoutManager n <$> mkReaper defaultReaperSettings
    { reaperAction = mkListAction prune
    , reaperDelay  = n * 1000
    }
  where
    prune a = do
        s <- atomically $ do
            x <- readTVar (state a)
            writeTVar (state a) (newState x)
            return x
        case s of
            Running 0 -> do
                ignore (action a)
                return Nothing
            Canceled -> return Nothing
            _        -> return $ Just a

    newState (Running k) = Running (k - 1)
    newState s           = s

destroy :: TimeoutManager -> Bool -> IO ()
destroy tm exec = mask_ $ do
    a <- reaperStop (reaper tm)
    when exec $ mapM_ f a
  where
    f e = readTVarIO (state e) >>= \s -> case s of
        Running _ -> ignore (action e)
        Canceled  -> return ()

add :: TimeoutManager -> Milliseconds -> IO () -> IO Action
add tm (Ms n) a = do
    r <- Action a <$> newTVarIO (Running $ n `div` roundtrip tm)
    reaperAdd (reaper tm) r
    return r

cancel :: Action -> IO ()
cancel a = atomically $ writeTVar (state a) Canceled

withTimeout :: TimeoutManager -> Milliseconds -> IO () -> IO a -> IO a
withTimeout tm m x a = bracket (add tm m x) cancel $ const a
