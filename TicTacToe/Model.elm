module TicTacToe.Model where

import Array exposing(Array)

-- players
type Player = PlayerOne | PlayerTwo

type Slot = Empty | PlayedBy Player

type BoardState = GameOn | Stalled | Won Player

-- board
type alias Board = Array (Array (Slot))

-- position (row, column)
type alias Position = (Int, Int)

newBoard : Board
newBoard = Array.initialize 3 (\row -> Array.initialize 3 (\col -> Empty))


-- MODEL
type alias Model = {board : Board, turn: Player, state: BoardState}

newModel : Model
newModel = {board = newBoard, turn = PlayerOne, state = GameOn}


switchTurns : Player -> Player
switchTurns player = 
    case player of
        PlayerOne -> PlayerTwo
        PlayerTwo -> PlayerOne

get : Board -> Position -> Maybe Slot
get board (row, column) =
    Array.get row board `Maybe.andThen` Array.get column


set : Board -> Position -> Player -> Board
set board (row, column) player = 
    case get board (row, column) of
        Nothing ->
            board
        Just Empty ->
            case Array.get row board of
                Nothing ->
                    board
                Just oldRow ->
                    let
                        newRow = Array.set column (PlayedBy player) oldRow
                    in
                        Array.set row newRow board
        Just s ->
            board
