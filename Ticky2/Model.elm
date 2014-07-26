module Ticky2.Model where

import Dict
import Set

type GameState = {
  board : Board ,
  currentPlayer : Marker,
  win : Maybe (Set.Set Pos)
}

data Marker = X | O

type Pos   = Int
type Board = Dict.Dict Pos Marker
type Win   = [Pos]

wins : [Win]
wins = [ [1, 2, 3] , [4, 5, 6] , [7, 8, 9]
       , [1, 4, 7] , [2, 5, 8] , [3, 6, 9]
       , [1, 5, 9] , [3, 5, 7]
       ]

initialState : GameState
initialState = { board = Dict.empty, currentPlayer = X, win = Nothing}

toggle : Marker -> Marker
toggle m =
  case m of
    X -> O
    O -> X

gameOver : GameState -> Bool
gameOver state = case state.win of
  (Just _) -> True
  Nothing  -> False

invalidMove : Pos -> GameState -> Bool
invalidMove pos state = Dict.member pos state.board

checkWin : GameState -> Win -> Maybe Win
checkWin state ps = let goal = Just (toggle state.currentPlayer)
                        won  = all (\marks -> marks == goal)
                                   (map (flip Dict.get state.board) ps)
                    in if won then Just ps else Nothing

firstWin : Maybe Win -> Maybe Win -> Maybe Win
firstWin ma mb = case (ma, mb) of
  (Just a, _) -> Just a
  (_, Just b) -> Just b
  otherwise   -> Nothing

updateWin : GameState -> GameState
updateWin state = let possibleWin = map (checkWin state) wins |> foldl firstWin Nothing
                  in case possibleWin of
                     (Just win) -> { state | win <- Just (Set.fromList win) }
                     otherwise  -> state

makePlay : Pos -> GameState -> GameState
makePlay pos state = { state | board <- Dict.insert pos state.currentPlayer state.board
                             , currentPlayer <- toggle state.currentPlayer
                             }

isWinningMark : Pos -> GameState -> Bool
isWinningMark idx state = case state.win of
  Just set -> Set.member idx set
  Nothing  -> False
