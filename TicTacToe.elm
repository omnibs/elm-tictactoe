module TicTacToe where

import Debug
import Set
import Array exposing(Array)
import String
import Char

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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


detectState : Player -> Board -> BoardState
detectState player board =
    List.map (getSlots board) victoryConditions
    |> List.map slotsToState
    |> (\states -> 
        if List.all (\state -> state == Stalled) states then
            Stalled
        else
            findVictory states
    )

findVictory : List BoardState -> BoardState
findVictory states = List.foldl (\state acc ->
                case state of
                    Stalled -> acc
                    GameOn -> acc
                    Won p -> Won p
            ) GameOn states

{-- gets slots from the board in the indicated positions. this is made to be curried --}
getSlots : Board -> List Position -> List Slot
getSlots board positions = 
    List.foldr (\position acc -> 
        case get board position of
            Nothing -> acc
            Just slot -> slot :: acc
    ) [] positions

{--
    If 3 are filled with same player -> Won
    If played by one and two -> Stalled
--}
slotsToState : List Slot -> BoardState
slotsToState slots =
    let
        nonEmpty = slots |> List.map getPlayer |> justPlayers
        anyPlayer = nonEmpty |> List.head
        numPlayers = nonEmpty |> List.map toString |> Set.fromList |> Set.size
    in
        case anyPlayer of
            Nothing -> GameOn
            Just player ->
                if numPlayers > 1 then -- more than one player = Stall
                    Stalled
                else if (List.length nonEmpty) == (List.length slots) then -- all slots played by same guy = won
                    Won player
                else
                    GameOn

getPlayer : Slot -> Maybe (Player)
getPlayer s =
    case s of
        Empty -> Nothing
        PlayedBy p -> Just p

{-- Filter list returning just the players instead of Maybe Player --}
justPlayers : List (Maybe Player) -> List Player
justPlayers list =
    list
    |> List.foldl (\ elm acc -> 
        case elm of
            Just p -> p :: acc
            Nothing -> acc
    ) []

{-- 
    Map definitions of row, line and diagonal into some sort of `fieldToState` function
    which returns `GameOn` `Stalled` `Won`. 

    If all = Stalled game is stalled, if any = Won game is won. 
--}

rows : List (List Position)
rows = 
    [
        [(0,0),(0,1),(0,2)],
        [(1,0),(1,1),(1,2)],
        [(2,0),(2,1),(2,2)]
    ]

columns : List (List Position)
columns = 
    [
        [(0,0),(1,0),(2,0)],
        [(0,1),(1,1),(2,1)],
        [(0,2),(1,2),(2,2)]
    ]

diagonals : List (List Position)
diagonals =
    [
        [(0,0),(1,1),(2,2)],
        [(2,0),(1,1),(0,2)]
    ]

victoryConditions = List.concat [rows, columns, diagonals]

-- ACTION
type Action = Reset | Play Position


update : Action -> Model -> Model
update action model =
    case action of
        Reset -> 
            newModel
        Play position -> 
            case model.state of
                Won p -> model
                Stalled -> model
                GameOn ->
                    play model position

play : Model -> Position -> Model
play model position =
    let 
        boardAfterPlay = set model.board position model.turn
        stateAfterPlay = detectState model.turn boardAfterPlay
        newTurn = switchTurns model.turn
    in
        {model | board = boardAfterPlay, turn = newTurn, state = stateAfterPlay}

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        h1 [] [text (stateDescription model)],
        table [style [("border", "1px solid black"), ("text-align", "center"), ("font-size", "70px")]] (Array.indexedMap (toTableRow address) model.board |> Array.toList)
    ]

toTableRow : Signal.Address Action -> Int -> Array Slot -> Html
toTableRow address rowIdx row =
    tr [] (Array.indexedMap (toTableCell address rowIdx) row |> Array.toList)

toTableCell : Signal.Address Action -> Int -> Int -> Slot -> Html
toTableCell address rowIdx colIdx slot =
    td [style [("border", "1px solid black"), ("width", "100px"), ("height", "100px")], onClick address (Play (rowIdx, colIdx))] 
        [text (case slot of 
                PlayedBy PlayerOne -> Char.fromCode 10060 |> String.fromChar
                PlayedBy PlayerTwo -> Char.fromCode 8413 |> String.fromChar
                Empty -> "")]

stateDescription : Model -> String
stateDescription model =
    case model.state of
        GameOn ->
            (toString model.turn) ++ "'s turn"
        Stalled -> "It's a Draw"
        Won p ->
            (toString p) ++ " Won"