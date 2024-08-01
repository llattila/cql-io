module Main (main) where

import Test.Tasty
import Test.Database.CQL.IO
import Test.Database.CQL.IO.Jobs
import System.Process
import System.Exit
import Control.Concurrent
import Database.CQL.IO
import Control.Monad.Catch
import Test.Database.CQL.IO.Pure

main :: IO ()
main =  runTests (ExitSuccess, "", "")

runTests :: (ExitCode, String, String) -> IO ()
runTests (exitCode, _, _) = do
    case exitCode of
      ExitFailure _ -> print "Cannot start Cassandra using Docker, trying to run test-cases without"
      _ -> pure () 
    tree <- sequence
        [ Test.Database.CQL.IO.tests
        , pure Test.Database.CQL.IO.Jobs.tests
        , pure Test.Database.CQL.IO.Pure.tests
        ]
    defaultMain $ testGroup "cql-io" tree
