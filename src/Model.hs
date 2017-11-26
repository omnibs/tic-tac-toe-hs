module Model where

import qualified Data.Map as Map

data Player
  = Player1
  | Player2
  deriving (Eq, Ord, Show)

type Position = Int

type Moves = Map.Map Int Player
