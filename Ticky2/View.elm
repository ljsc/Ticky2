module Ticky2.View where

import Ticky2.Model (..)
import Ticky2.Model as M

import Dict
import Set
import Text (centered, defaultStyle, toText, style)

boardScale : Float
boardScale = 0.9

tscale : Float -> Int -> Int
tscale y x = toFloat x * y |> floor

square : Int -> Color -> Maybe Marker -> Element
square size c m = let s = toFloat size
                      parts =  [ filled black <| rect s s
                               , filled white <| rect (s-1) (s-1)
                               , scale 15.0   <| toForm
                                              <| centered
                                              <| style { defaultStyle | color <- c }
                                              <| toText
                                              <| showMark m
                               ]
                  in collage size size parts

showMark : Maybe Marker -> String
showMark m = case m of
  Just mark -> show mark
  Nothing   -> ""

renderGame : (Int, Int) -> GameState -> Element
renderGame (w,h) state =
  let s      = tscale (boardScale * 0.33) h
      sq idx = square s (sqColor idx state) (Dict.get idx state.board)
      row cells   = flow right (map sq cells)
  in container w h middle <| row [1,2,3] `above` row [4,5,6] `above` row [7,8,9]

sqColor : M.Pos -> GameState -> Color
sqColor idx state = if isWinningMark idx state then green else black

