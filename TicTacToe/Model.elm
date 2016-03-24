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
type alias Model = {board : Board, turn: Player, state: BoardState, winningMove: Maybe (List Position)}

newModel : Model
newModel = {board = newBoard, turn = PlayerOne, state = GameOn, winningMove = Nothing}


switchTurns : Player -> Player
switchTurns player = 
    case player of
        PlayerOne -> PlayerTwo
        PlayerTwo -> PlayerOne

get : Board -> Position -> Maybe Slot
get board (row, column) =
    Array.get row board `Maybe.andThen` Array.get column

applyAt : Int -> (a -> a) -> Array a -> Array a
applyAt index f array =
    case Array.get index array of
        Nothing -> array
        Just element -> Array.set index (f element) array

setSlotPlayer : Player -> Slot -> Slot
setSlotPlayer player slot =
    case slot of
        Empty -> PlayedBy player
        whatevs -> whatevs

set : Position -> Player -> Board -> Board
set (row, column) player board = 
    board
    |> applyAt row (applyAt column (setSlotPlayer player))
