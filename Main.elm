import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (Generator, Seed, generate)

type alias State = { x : Float
                   , y : Float
                   , vx : Float
                   , vy : Float
                   , size : Float
                   , color : Color
                   }

type alias Mass = Float

type alias Position = (Float, Float)

g : Float
g = 0.00002

width : Float
width = 800

height : Float
height = 800

particleMass : Float
particleMass = 10

attractorMass : Float
attractorMass = 10

attractorX : Float
attractorX = 0

attractorY : Float
attractorY = 0


main : Signal Element
main =
  let
  --{ x = -200, y = 0, vx = 0.02, vy = -0.02, size = 20, color = darkPurple }
    start = [ { x = 200, y = 0,  vx = 0.02, vy = 0.04, size = 5, color = charcoal }
            , { x = 200, y = 20, vx = -0.02, vy = 0.04, size = 5, color = charcoal }
            , { x = 200, y = 40, vx = 0.02, vy = 0.04, size = 5, color = charcoal }
            , { x = 200, y = 60, vx = -0.02, vy = 0.04, size = 5, color = charcoal }
            , { x = 200, y = 80, vx = 0.02, vy = 0.04, size = 5, color = charcoal }
            , { x = -100, y = 0,  vx = -0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = -100, y = 20, vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = -50, y = 40, vx = -0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = -100, y = 60, vx = 0.02, vy = -0.04, size = 5, color = charcoal }
            , { x = -100, y = 80, vx = -0.02, vy = -0.04, size = 5, color = charcoal }
            ]
    accumulated = Signal.foldp gravitateAll start (fps 60)
  in
    Signal.map draw accumulated


genParticle : Random.Seed -> (State, Random.Seed)
genParticle s0 =
  let
    v = Random.float -0.05 0.05
    (vx, s1) = generate v s0
    (vy, s2) = generate v s1

    pos = Random.float -200 200
    (x, s3) = generate pos s2
    (y, s4) = generate pos s3

    p = { x = x, y = y, vx = vx, vy = vy, size = 5, color = charcoal }
  in
    (p, s4)



ball : State -> Form
ball s =
  filled s.color (circle s.size)
    |> move (s.x, s.y)


draw : List State -> Element
draw states =
  collage (round width) (round height)
    (filled darkGrey (rect width height) :: (filled grey (rect 5 5)) :: List.map ball states)



gravitateAll : Time -> List State -> List State
gravitateAll t states =
  --List.map (gravitate t (attractorX, attractorY) attractorMass) states
  let
    center = findCenter states
    centered = List.map (recenter center) states
    sorted = List.sortBy (distance center) centered
  in
    nbody t center sorted

recenter : Position -> State -> State
recenter (dx, dy) s = { s | x = s.x - dx, y = s.y - dy }

distance : Position -> State -> Float
distance (x, y) s = sqrt ((x - s.x)*(x - s.x) + (y - s.y)*(y - s.y))

findCenter : List State -> Position
findCenter states =
  let
    n = toFloat ( List.length states )
    x = List.sum ( List.map .x states ) / n
    y = List.sum ( List.map .y states ) / n
  in
    (x, y)

-- The list of particles should be sorted by distance from the center
nbody : Time -> Position -> List State -> List State
nbody t center particles =
  let
    updateParticle s n = gravitate t center ((toFloat n)*particleMass) s
  in
    mapWithIndex updateParticle particles

mapWithIndex : (a -> Int -> b) -> List a -> List b
mapWithIndex fn items =
  let
    op item (res, index) = (fn item index :: res, index + 1)
    folded = List.foldl op ([], 0) items
  in
    fst folded

gravitate : Time -> Position -> Mass -> State -> State
gravitate t (attractorX, attractorY) attractorMass s =
  let
    dx  = s.x - attractorX
    dy  = s.y - attractorY
    rSq = (abs dx) + (abs dy)
    a   = -g * attractorMass / rSq
    r   = sqrt rSq
    ax  = a * dx / r
    ay  = a * dy / r

    vx' = s.vx + ax*t
    x'  = s.x + (s.vx + vx')/2*t

    vy' = s.vy + ay*t
    y'  = s.y + (s.vy+vy')/2*t
  in
    { s | x = x'
        , y = y'
        , vx = vx'
        , vy = vy'
    }
