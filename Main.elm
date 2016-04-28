import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias State = { x : Float
                   , y : Float
                   , vx : Float
                   , vy : Float
                   , size : Float
                   , color : Color
                   }

g : Float
g = 0.00002

width : Float
width = 800

height : Float
height = 800

attractorMass : Float
attractorMass = 10

attractorX : Float
attractorX = 0

attractorY : Float
attractorY = 0


main : Signal Element
main =
  let
    start = [ { x = -200, y = 0, vx = 0.02, vy = -0.02, size = 20, color = darkPurple }
            , { x = 200, y = 0,  vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = 200, y = 20,  vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = 200, y = 40,  vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = 200, y = 60,  vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = 200, y = 80,  vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            ]
    accumulated = Signal.foldp gravitateAll start (fps 60)
  in
    Signal.map draw accumulated


ball : State -> Form
ball s =
  filled s.color (circle s.size)
    |> move (s.x, s.y)


draw : List State -> Element
draw states =
  collage (round width) (round height)
    (filled darkGrey (rect width height) :: List.map ball states)


gravitateAll : Time -> List State -> List State
gravitateAll t states = List.map (gravitate t) states


gravitate : Time -> State -> State
gravitate t s =
  let
    dx  = s.x - attractorX
    dy  = s.y - attractorY
    rSq = (abs dx) + (abs dy)
    a   = -g * attractorMass / rSq
    r   = sqrt rSq
    ax  = a * dx / r
    ay  = a * dy / r

    vx' = s.vx + ax*t
    x'  = s.x + vx'*t

    vy' = s.vy + ay*t
    y'  = s.y + vy'*t
  in
    { s | x = x'
        , y = y'
        , vx = vx'
        , vy = vy'
    }
