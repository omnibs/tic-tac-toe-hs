module GameOver where

import Data.List
import qualified Data.Map as Map
import Model

data State
  = GameOn
  | Victory
  | Draw

gameOver :: Int -> Moves -> State
gameOver size moves =
  let conditions :: [Int -> Moves -> Bool]
      conditions = [rowVictory, columnVictory, diagonalVictory]
      victory :: Bool
      victory = or $ conditions <*> [size] <*> [moves]
  in if victory
       then Victory
       else if draw size moves
              then Draw
              else GameOn

samePlayer :: [Maybe Player] -> Bool
samePlayer players =
  all (\x -> x /= Nothing && x == (head players)) (tail players)

rowIndexes :: Int -> [[Int]]
rowIndexes size =
  let baseRow = [0 .. size - 1]
      makeCols :: Int -> [Int]
      makeCols rowIdx = map (\col -> col + size * rowIdx) baseRow
  in map makeCols baseRow :: [[Int]]

colIndexes :: Int -> [[Int]]
colIndexes size = transpose $ rowIndexes size

diagIndexes :: Int -> [[Int]]
diagIndexes size =
  let baseIdx = [0 .. size - 1]
      rowColToIdx a b = a * size + b
      mainDiag = zipWith rowColToIdx baseIdx baseIdx
      altDiag = zipWith rowColToIdx baseIdx (reverse baseIdx)
  in [mainDiag, altDiag]

maybePlayers :: Moves -> [[Int]] -> [[Maybe Player]]
maybePlayers moves indexMatrix =
  map (\row -> map (\idx -> Map.lookup idx moves) row) indexMatrix

rowVictory :: Int -> Moves -> Bool
rowVictory size moves =
  let rows = rowIndexes size
  in any samePlayer $ maybePlayers moves rows

columnVictory :: Int -> Moves -> Bool
columnVictory size moves =
  let cols = colIndexes size
  in any samePlayer $ maybePlayers moves cols

-- [0,1,2,3] [0,1,2,3] -> [0, 5, 10, 15]
diagonalVictory :: Int -> Moves -> Bool
diagonalVictory size moves =
  let playersOnPositions = maybePlayers moves (diagIndexes size)
  in any samePlayer playersOnPositions

bothPlayers :: [Maybe Player] -> Bool
bothPlayers positions =
  let justPlayer player = filter (== Just player)
      hasPlayer player = length $ justPlayer player positions
      playerOccurrences = hasPlayer <$> [Player1, Player2]
  in all (> 0) playerOccurrences

draw :: Int -> Moves -> Bool
draw size moves =
  let diagPlayers = maybePlayers moves (diagIndexes size)
      rowPlayers = maybePlayers moves (rowIndexes size)
      colPlayers = maybePlayers moves (colIndexes size)
      patterns = [diagPlayers, rowPlayers, colPlayers]
      results :: [Bool]
      results = all bothPlayers <$> patterns
  in and $ results
