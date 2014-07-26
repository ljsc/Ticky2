{-
Ticky2 - Simple Tic-tac-toe implementation in Elm
Copyright (C) 2014 Louis J. Scoras

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Ticky2 where

import Ticky2.Model (..)
import Ticky2.Model as Model
import Ticky2.View (..)

import Mouse
import Window

data Action = Noop | Play Model.Pos | NewGame

actions : Signal Action
actions =
  let toAction choice =
        case choice of
           Nothing -> Noop
           Just x  -> Play x
  in (\ds curs -> trans ds curs |> toAction) <~ Window.dimensions
                                             ~ sampleOn Mouse.clicks Mouse.position

step : Action -> GameState -> GameState
step action state =
  case action of
    Noop -> state
    Play pos ->
      if gameOver state || invalidMove pos state
      then state
      else updateWin . makePlay pos <| state

-- TODO: Make this not horrible ;)
trans : (Int,Int) -> (Int, Int) -> Maybe Int
trans (w,h) (x,y) =
  let bs = tscale boardScale h
      ss = tscale 0.33 bs

      bound m pos =
        let go n pos' = if | n > 3 -> Nothing
                           | pos' < 0 && n > 0 -> Just n
                           | otherwise -> go (n+1) (pos'-ss)
        in go 0 (pos - m)

      margin s      = tscale 0.5 (s - bs)
      row           = bound (margin w) x
      col           = bound (margin h) y
      normalize x y = Just <| x + 3*(y-1)

  in case (row, col) of
       (Just x', Just y') -> normalize x' y'
       otherwise          -> Nothing

main : Signal Element
main = renderGame <~ Window.dimensions ~ foldp step initialState actions
