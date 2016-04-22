import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias State = { position : (Float, Float)
                   , velocity : (Float, Float)
                   , size : Float
                   , color : Color
                   }

width : Float
width = 800

height : Float
height = 800


main : Signal Element
main =
  let
    start = [ { position = (-50,0), velocity = (0.2,-0.1), size = 20, color = darkPurple }
            , { position = (50,0),  velocity = (0.2,-0.4), size = 10, color = charcoal }
            ]
    accumulated = Signal.foldp bounce start (fps 60)
  in
    Signal.map draw accumulated


ball : State -> Form
ball s =
  filled s.color (circle s.size)
    |> move (fst s.position, snd s.position)


draw : List State -> Element
draw states =
  collage (round width) (round height)
    (filled darkGrey (rect width height) :: List.map ball states)


bounce : Time -> List State -> List State
bounce t states =
  let updater = updateState t in
    List.map updater states


updateState : Time -> State -> State
updateState t s =
  let
    (newX, newVX) = updatePosition t (fst s.position, fst s.velocity) (width/2 - s.size)
    (newY, newVY) = updatePosition t (snd s.position, snd s.velocity) (height/2 - s.size)
  in
    { s | position = (newX, newY)
        , velocity = (newVX, newVY)
    }


updatePosition : Time -> (Float, Float) -> Float -> (Float, Float)
updatePosition t (x, v) bound =
  let
    projected = x + t*v
    extra     = (abs projected) - bound
    newV      = if extra <= 0 then v else -v
    newX      = if extra <= 0 then projected else projected + 2*newV*extra
  in
    (newX, newV)