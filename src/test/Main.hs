module Main (main) where

import Test.Tasty
import Test.Database.CQL.IO
import Test.Database.CQL.IO.Jobs
import System.Process
import System.Exit
import Control.Concurrent
import Database.CQL.IO
import Control.Monad.Catch

main :: IO ()
main = bracket startCassandra shutdownCassandra runTests

startCassandra :: IO (ExitCode, String, String)
startCassandra = readProcessWithExitCode "docker-compose" ["up", "-d"] ""

shutdownCassandra :: (ExitCode, String, String) -> IO ()
shutdownCassandra _ = do
  _ <- readProcessWithExitCode "docker-compose" ["down"] ""
  pure ()

runTests :: (ExitCode, String, String) -> IO ()
runTests (exitCode, _, _) =
    case exitCode of
      ExitFailure _ -> undefined
      _ -> do
        waitUntilCassandraIsRunning "cassandra-dev"
        tree <- sequence
            [ Test.Database.CQL.IO.tests
            , pure Test.Database.CQL.IO.Jobs.tests
            ]
        defaultMain $ testGroup "cql-io" tree

waitUntilCassandraIsRunning :: String -> IO ()
waitUntilCassandraIsRunning containerID = do
  (exitCode, _, _) <- readProcessWithExitCode "docker" ["exec", containerID, "/opt/cassandra/bin/nodetool", "status"] ""
  case exitCode of
    ExitFailure _ -> do
      print "Waiting for Cassandra"
      threadDelay 1000000
      waitUntilCassandraIsRunning containerID
    _ -> do
      conn <- waitUntilClientConnects
      shutdown conn

waitUntilClientConnects :: IO ClientState
waitUntilClientConnects = catch (Database.CQL.IO.init defSettings) waitExtra

waitExtra :: SomeException -> IO ClientState
waitExtra _ = do
  threadDelay 1000000
  waitUntilClientConnects