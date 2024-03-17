{-# LANGUAGE ScopedTypeVariables #-}

module Test.Database.CQL.IO.Pure where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Property (succeeded, failed, reason)
import qualified Database.CQL.IO.Client as C 
import Data.GenValidity
import Data.Validity
import Data.Int
import Data.List
import Data.Ord
import Data.Function
import Database.CQL.Protocol
import Test.Validity
import qualified Data.Map.Strict as M
import Data.Maybe

tests :: TestTree
tests = testGroup "Partition by token"
  [ QC.testProperty "Partition amount equal or less than amount of hosts" $
    forAll (chooseInt (1,12)) $ \hostCount -> 
      forAll (createHostTokenList hostCount) $ \hostTokens ->
        forAllValid  $ \(routingTuples :: [(RoutingToken, [Int64])]) ->
          let largestHostId = snd $ C.largestHost hostTokens
              partitioned = C.partitionByTokenHelper hostTokens routingTuples largestHostId M.empty
          in if length partitioned <= hostCount
               then succeeded
               else failed { Test.QuickCheck.Property.reason = "Partitioned too large:\n" <> show partitioned},
    QC.testProperty "All partitions have the same closest" $
    forAll (chooseInt (1,12)) $ \hostCount -> 
      forAll (createHostTokenList hostCount) $ \hostTokens ->
        forAll (genValid `suchThat`(\x -> length x >= 2)) $ \routingInts  ->
          let routingTokenTuples = map (\x -> (RoutingToken x, x)) routingInts
              largestHostId = snd $ C.largestHost hostTokens
              sortedMapped = M.assocs (foldl C.addToPartitionMap M.empty routingTokenTuples)
              partitioned = C.partitionByTokenHelper hostTokens sortedMapped largestHostId M.empty
              tupled = M.assocs partitioned
              concatted = concatMap (\(x,y) -> zip (map RoutingToken y) (repeat x)) tupled
              isClosest = map (checkClosest hostTokens) concatted
          in if all (\x -> isNothing x) isClosest
                then succeeded
                else failed { Test.QuickCheck.Property.reason = intercalate "\n" (map show concatted) <> "\n" <> show isClosest}
  ]
--partitionByToken :: Policy -> [(RoutingToken, b)] -> IO (Map.Map Host [b])
instance Validity RoutingToken

instance GenValid RoutingToken

createHostTokenList :: Int -> Gen [(Int64, Int)]
createHostTokenList hostCount = do
   listOfTokens <- nub <$> infiniteListOf genValid
   pure $ sortBy (compare `on` fst) $ extractAllLists listOfTokens [1..hostCount] []

extractAllLists :: [Int64] -> [Int] -> [(Int64, Int)] -> [(Int64, Int)]
extractAllLists _ [] previous = previous
extractAllLists uniques (x:xs) previous =
   let (toAdd, rest) = extractPartOfList uniques x 
   in extractAllLists rest xs $ previous ++ toAdd

extractPartOfList :: [Int64] -> Int -> ([(Int64, Int)], [Int64])
extractPartOfList listOfUniques hostId = 
   let (forHost, rest) = splitAt 16 listOfUniques
       hostTokens = zip forHost $ repeat hostId
   in (hostTokens, rest)

checkClosest :: [(Int64, Int)] -> (RoutingToken, Int) -> Maybe (Int64, Int)
checkClosest x tok = checkClosestHelper x tok $ C.largestHost x

checkClosestHelper :: [(Int64, Int)] -> (RoutingToken, Int) -> (Int64, Int) -> Maybe (Int64, Int)
checkClosestHelper [] (_, sel) (dist, best) = if sel == best then Nothing else Just (dist, best )
checkClosestHelper xs (RoutingToken rt, sel) (smallDist, smallBest) =
    let x = head xs
    in if rt >= fst x
      then checkClosestHelper (tail xs) (RoutingToken rt, sel) (rt - fst x, snd x)
      else checkClosestHelper (tail xs) (RoutingToken rt, sel) (smallDist, smallBest)


