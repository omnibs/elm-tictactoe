module TicTacToe.VictoryConditions where

import TicTacToe.Model exposing(..)
import Set

detectState : Player -> Board -> (BoardState, Maybe (List Position))
detectState player board =
    victoryConditions
    |> List.foldl (\positions (accState, winningPositions) ->
        let 
            state =
                positions
                |> getSlots board
                |> slotsToState
        in
            case state of
                Won p -> 
                    (Won p, Just positions)
                _ ->
                    ((if accState == Stalled then state else accState), Nothing)
    ) (Stalled, Nothing)

isWinningMove : Maybe (List Position) -> Position -> Bool
isWinningMove list pos =
    case list of
        Nothing -> False
        Just l -> List.member pos l

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
