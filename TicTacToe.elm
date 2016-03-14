module TicTacToe where

import Array
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

-- players
type Player = PlayerOne | PlayerTwo

type Slot p = Empty | Player p

-- board
type alias Board p = Array.Array (Array.Array (Slot p))

type alias Position = (Int, Int)

initialize : Board p
initialize = Array.initialize 3 (\row -> Array.initialize 3 (\col -> Empty))

-- MODEL
type alias Model p = {board : Board p, turn: Player}

-- ACTION
type Action pos = Reset | Play Position pos

update : Action pos -> Model p -> Model p
update action model =
    case action of
        Reset -> 
            { model | board = initialize, turn = PlayerOne }
        Play row col -> play model.board model.player (row, col)

play : Board p -> Player -> Position -> Model q
play board player row col =
    {board = initialize, turn = PlayerOne}

-- VIEW


{-- 

x how to instantiate a board?
how to change an item in an array?
how to map screen board to Board?
how to model end of game rules?

Array.initialize numRows (\row -> Array.initialize numCols (\col -> Empty))
--}

