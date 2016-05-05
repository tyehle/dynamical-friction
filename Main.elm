import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (Generator, Seed, generate)
import Window exposing (width, height)

type alias World = ( State, List State )

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

particleMass : Float
particleMass = 0.5

attractorMass : Float
attractorMass = 10


main : Signal Element
main = Signal.map2 draw Window.dimensions simulate


ball : State -> Form
ball s =
  filled s.color (circle s.size)
    |> move (s.x, s.y)


draw : (Int, Int) -> World -> Element
draw (width, height) (mass, particles) =
  collage width height
    (filled charcoal (rect (toFloat width) (toFloat height)) :: (filled lightBlue (rect 5 5)) :: List.map ball (mass :: particles))



simulate : Signal World
simulate =
  let
    mass  = { x = -200, y = 0, vx = 0.02, vy = 0, size = 20, color = lightPurple }
    start = genParticles 50 (Random.initialSeed 2) []
  in
    Signal.foldp gravitateAll (mass, start) (fps 60)

genParticles : Int -> Seed -> List State -> List State
genParticles n s ps =
  if n <= 0 then
    ps
  else
    let
      (p, s') = genParticle s
    in
      genParticles (n-1) s' (p :: ps)

genParticle : Seed -> (State, Seed)
genParticle s0 =
  let
    theta = Random.float 0 (2*pi)

    v = Random.float -0.05 0.05
    (vr, s1) = generate v s0
    (vt, s2) = generate theta s1
    vx = vr * cos vt
    vy = vr * sin vt

    pos = Random.float -200 200
    (r, s3) = generate pos s2
    (t, s4) = generate theta s3
    x = r * cos t
    y = r * sin t

    p = { x = x, y = y, vx = vx, vy = vy, size = 3, color = grey }
  in
    (p, s4)



gravitateAll : Time -> World -> World
gravitateAll t (mass, particles) =
  --List.map (gravitate t (attractorX, attractorY) attractorMass) states
  let
    recenter (dx, dy) s = { s | x = s.x - dx, y = s.y - dy }
    distance (x, y) s = sqrt ((x - s.x)*(x - s.x) + (y - s.y)*(y - s.y))

    center = findCenter particles
    centered = List.map (recenter center) particles
    sorted = List.sortBy (distance center) centered
  in
    nbody t center (mass, sorted)

findCenter : List State -> Position
findCenter states =
  let
    n = toFloat ( List.length states )
    x = List.sum ( List.map .x states ) / n
    y = List.sum ( List.map .y states ) / n
  in
    (x, y)

-- The list of particles should be sorted by distance from the center
nbody : Time -> Position -> World -> World
nbody t center (mass, particles) =
  let
    updateParticle s n = accelerate t (acceleration t center ((toFloat n)*particleMass) s) s
  in
    (mass, mapWithIndex updateParticle particles)

mapWithIndex : (a -> Int -> b) -> List a -> List b
mapWithIndex fn items =
  let
    op item (res, index) = (fn item index :: res, index + 1)
    folded = List.foldl op ([], 0) items
  in
    fst folded

acceleration : Time -> Position -> Mass -> State -> Position
acceleration t (x, y) m s =
  let
    dx  = s.x - x
    dy  = s.y - y
    rSq = (abs dx) + (abs dy)
    a   = -g * attractorMass / rSq
    r   = sqrt rSq
    ax  = a * dx / r
    ay  = a * dy / r
  in
    (ax, ay)

accelerate : Time -> Position -> State -> State
accelerate t (ax, ay) s =
  let
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
