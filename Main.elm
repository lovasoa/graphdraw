module GraphDraw where

import Array
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse


-- MODEL

type alias Point = (Float,Float)
type alias Points = List Point
type alias Arrow = (Point,Point)
type alias Model = {
  points: Points,
  arrows: List Arrow,
  curLineStart: Maybe Point,
  mouse: Point,
  selected: Maybe Point
}

startModel = {
  points=[],
  arrows=[],
  curLineStart = Nothing,
  mouse = (0,0),
  selected = Nothing
 }

-- UPDATE

type Action = Pressed Bool | Move Point

update : Action -> Model -> Model
update action model =
  case action of
    Pressed isdown ->
      if isdown then
        case hovered model of
          Just p -> {model | curLineStart = Just p}
          Nothing -> {model |
                        points = model.mouse::model.points,
                        curLineStart = Just model.mouse
                      }
      else
        case model.curLineStart of
          Just p ->
            let
              hoveredP = hovered model
              q = Maybe.withDefault model.mouse (hovered model)
            in
            {model |
              curLineStart = Nothing,
              arrows = (p,q)::model.arrows,
              points = if hoveredP == Nothing then q::model.points else model.points
            }
          Nothing -> model
    Move p -> {model | mouse = p}

radius = 8

distance: Point -> Point -> Float
distance (x,y) (a,b) = sqrt ((x-a)^2 + (y-b)^2)

isInCircle: Point -> Point -> Bool
isInCircle a b =  distance a b < radius

--Find the node pointed by the mouse
hovered : Model -> Maybe Point
hovered m =
  let reducer point old = if isInCircle m.mouse point then Just point else old
  in List.foldr reducer Nothing m.points

-- VIEW
w = 1500
h = 900

view : Model -> Element
view model =
  let
    hoverP = hovered model
    isHovered point = case hoverP of
                        Just p -> p == point
                        Nothing -> False
  in
  collage w h
          (
            (showCurLine model) ++
            (List.map showArrow model.arrows) ++
            (List.map (\point -> showPoint (isHovered point) point) model.points)
          )


showPoint: Bool -> (Float, Float) -> Form
showPoint hovered point =
  circle radius
    |> filled (if hovered then red else blue)
    |> move point

showArrow : Arrow -> Form
showArrow = uncurry drawLine

showCurLine: Model -> List Form
showCurLine model = case model.curLineStart of
  Just p -> [drawLine p model.mouse]
  Nothing -> []

drawLine : Point -> Point -> Form
drawLine p q =
  segment p q
  |> traced defaultLine

convertCoords : (Int, Int) -> (Float,Float)
convertCoords (x,y) = (-w/2 + toFloat x, h/2 - toFloat y)

mousepos = Signal.map convertCoords Mouse.position

main =
  Signal.foldp update startModel
   (Signal.mergeMany [Signal.map Move mousepos, Signal.map Pressed Mouse.isDown])
  |> Signal.map view
