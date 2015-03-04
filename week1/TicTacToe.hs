{-
  Example code produced "during" Advanced Programming lecture.

  Code for tic-tac-toe games

  Date: Sep 2, 2014
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}


data Player = Cross | Nought
            deriving (Show, Eq)

data Cell = Move Player | Empty
          deriving (Show, Eq)

type Row = ...
type Board = ...

emptyBoard :: Board
emptyBoard = undefined


type Position = (Int, Int)  -- Is this right??????

move :: Position -> Board -> Player -> Board
move _ _ _ = undefined



-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = undefined


makeMove :: Position -> GameState -> GameState
makeMove _ _ = undefined

validMove :: Position -> GameState -> Bool
validMove _ _ = undefined

allMoves :: [Position]
allMoves = undefined

allValidMoves :: GameState -> [Position]
allValidMoves _ = undefined


-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)]


-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree _ = undefined


-- Return all game states in a game tree, duplicates are allowed
allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes.snd) subs

-- Observe the difference in running time of
--    length $ allNodes $ makeTree startState    -- should return 986410
-- and
--    take 3 $ allNodes $ makeTree startState
-- Can you explain that?
