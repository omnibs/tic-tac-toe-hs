module Lib
  ( someFunc
  ) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import GameOver
import Model

-- VIEW
boardStr :: Int -> Moves -> String
boardStr size moves =
  let fullBoard = [0 .. (size ^ 2 - 1)]
      slots :: [String]
      slots = fmap (slotStr moves) fullBoard
      lines :: [[String]]
      lines = chunkOf size slots
      linesWithBreaks :: [[String]]
      linesWithBreaks = map (\line -> line ++ ["\n"]) lines
      linesWithNumbers :: [[String]]
      linesWithNumbers =
        map (\(num, line) -> [(show num)] ++ line) (zip [1 ..] linesWithBreaks)
      oneRow = take size (map (\c -> [c]) ['a' .. 'z'])
      header = [" "] ++ oneRow ++ ["\n"]
      linesWithHeader = header : linesWithNumbers
      linesWithSpacing =
        map (\row -> row >>= (\col -> [" ", col])) linesWithHeader
      flatBoard :: [String]
      flatBoard = linesWithSpacing >>= (\x -> x)
  in intercalate "" flatBoard

slotStr :: Moves -> Int -> String
slotStr moves pos =
  let maybeMove = Map.lookup pos moves
  in case maybeMove of
       Just player -> playerStr player
       Nothing -> " "

playerStr :: Player -> String
playerStr player =
  case player of
    Player1 -> "o"
    Player2 -> "x"

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n l
  | n > 0 = (take n l) : (chunkOf n (drop n l))
  | otherwise = error "Negative n"

-- UPDATE
readMove :: Int -> IO (Position)
readMove size = do
  putStrLn "\nEnter a move:"
  move <- getLine :: IO String
  let digits = filter isDigit move :: String
  let row = read digits :: Int
  let nonDigits = filter (not . isDigit) move :: String
  let asInts = map (\c -> ord (toLower c) - ord 'a') nonDigits :: [Int]
  let col = sum asInts :: Int
  let position = (row - 1) * size + col :: Position
  if position > size ^ 2 - 1
    then do
      putStrLn "Invalid move"
      readMove size
    else return position

readValidMove :: Int -> Moves -> IO Position
readValidMove size moves = do
  position <- readMove size :: IO Position
  if any (== position) (Map.keys moves)
    then readValidMove size moves
    else return position

otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

gameLoop :: Int -> Moves -> Player -> IO ()
gameLoop size moves player = do
  putStrLn (boardStr size moves)
  move <- readValidMove size moves :: IO Position
  let newMoves = Map.insert move player moves
  case gameOver size newMoves of
    Victory -> do
      putStrLn (boardStr size newMoves)
      putStrLn $ (show player) ++ " Won!"
    Draw -> do
      putStrLn (boardStr size newMoves)
      putStrLn "It's a draw! No player can win this time."
    GameOn -> gameLoop size newMoves (otherPlayer player)

someFunc :: IO ()
someFunc = gameLoop 3 Map.empty Player1
