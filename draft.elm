module TicTacToe where

import Array

-- players
type Player = PlayerOne | PlayerTwo

type Slot p = Empty | Player p

-- board
type alias Board p = Array.Array (Array.Array Slot p)

type alias Position = (Int, Int)

initialize : () -> Board
initialize = Array.initialize 3 (\row -> Array.initialize 3 (\col -> Empty))

-- MODEL
type alias Model = {board : Board, turn: Player}

-- ACTION
type Action pos = Reset | Play Position pos

--update : Action -> Model -> Model
--update

-- VIEW

{-- 

x how to instantiate a board?
how to change an item in an array?
how to map screen board to Board?
how to model end of game rules?

Array.initialize numRows (\row -> Array.initialize numCols (\col -> Empty))
--}

