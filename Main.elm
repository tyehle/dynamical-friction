import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Time exposing (..)
import Random exposing (Generator, Seed, step)
import Window exposing (width, height)
import AnimationFrame
import Html.App as App
import Html exposing (..)
import Task

type alias World = { attractor : Particle
                   , attractorMass : Mass
                   , particles : List Particle
                   , particleMass : Mass
                   , windowSize : Window.Size
                   }

type alias Particle = { pos : Vec2
                      , velocity : Vec2
                      , size : Float
                      , color : Color
                      }

type alias Mass = Float

type alias Vec2 = { x : Float, y : Float }

type Message = Step Time | Resize Window.Size


g : Float
g = 0.00002

defaultSize : Int
defaultSize = 200


-- INIT --

init : (World, Cmd Message)
init = 
  let
    mass  = { pos = {x=-200, y=0}, velocity = {x=0.02, y=0}, size = 20, color = lightPurple }
    particles = genParticles 50 (Random.initialSeed 2)
    initialResize = Task.perform (\_ -> Resize {height=defaultSize, width=defaultSize}) Resize Window.size
  in
    ({attractor = mass, attractorMass = 2, particles = particles, particleMass = 0.5, windowSize = {height=defaultSize, width=defaultSize}}, initialResize)

genParticles : Int -> Seed -> List Particle
genParticles = genParticlesAccumulated []

genParticlesAccumulated : List Particle -> Int -> Seed -> List Particle
genParticlesAccumulated ps n s =
  if n <= 0 then
    ps
  else
    let
      (p, s') = genParticle s
    in
      genParticlesAccumulated (p :: ps) (n-1) s'

genParticle : Seed -> (Particle, Seed)
genParticle s0 =
  let
    theta = Random.float 0 (2*pi)

    v = Random.float -0.05 0.05
    (vr, s1) = step v s0
    (vt, s2) = step theta s1
    vx = vr * cos vt
    vy = vr * sin vt

    pos = Random.float -200 200
    (r, s3) = step pos s2
    (t, s4) = step theta s3
    x = r * cos t
    y = r * sin t

    p = { pos = {x=x, y=y}, velocity = {x=vx, y=vy}, size = 3.0, color = grey }
  in
    (p, s4)



-- VIEW --

view : World -> Html Message
view world = toHtml (drawWorld world)

drawBall : Particle -> Form
drawBall s =
  filled s.color (circle s.size)
    |> move (s.pos.x, s.pos.y)


drawWorld : World -> Element
drawWorld {attractor, particles, windowSize} =
  let
    {width, height} = windowSize
    background = filled charcoal (rect (toFloat width) (toFloat height))
    centerMark = filled lightBlue (rect 5 5)
    particleCircles = List.map drawBall (attractor :: particles)
  in
    collage width height
      (background :: centerMark :: particleCircles)



-- UPDATE --

update : Message -> World -> (World, Cmd Message)
update msg world = case msg of
  Step t -> (gravitateAll t world, Cmd.none)
  Resize size -> ({world | windowSize = size}, Cmd.none)

gravitateAll : Time -> World -> World
gravitateAll t world =
  let
    recenter {x,y} s = { s | pos = { x = s.pos.x - x, y = s.pos.y - y } }
    distance {x,y} s = sqrt ((x - s.pos.x)*(x - s.pos.x) + (y - s.pos.y)*(y - s.pos.y))

    {particles, attractor} = world

    center = findCenter particles
    centered = List.map (recenter center) particles
    sorted = List.sortBy (distance center) centered
  in
    nbody t center {world | particles = sorted}

findCenter : List Particle -> Vec2
findCenter particles =
  let
    n = toFloat ( List.length particles )
    x = List.sum ( List.map (\p -> p.pos.x) particles ) / n
    y = List.sum ( List.map (\p -> p.pos.y) particles ) / n
  in
    {x=x, y=y}

-- The list of particles should be sorted by distance from the center
nbody : Time -> Vec2 -> World -> World
nbody t center world =
  let
    {attractor, attractorMass, particles, particleMass} = world
    newAttractor = updateAttractor particleMass t particles attractor
    newParticles = List.indexedMap (updateParticle t attractorMass particleMass center attractor) particles
  in
    {world | attractor = newAttractor, particles = newParticles}


updateAttractor : Time -> Mass -> List Particle -> Particle -> Particle
updateAttractor t particleMass particles attractor =
  let
    accelForParticle p = acceleration t p.pos particleMass attractor
    totalAccel = List.foldl (\p a -> vecAdd a (accelForParticle p)) {x=0, y=0} particles
  in
    accelerate t totalAccel attractor


updateParticle : Time -> Mass -> Mass -> Vec2 -> Particle -> Int -> Particle -> Particle
updateParticle t attractorMass particleMass center attractor n s =
  let
    pa = acceleration t center ((toFloat n)*particleMass) s
    ma = acceleration t attractor.pos attractorMass s
  in
    accelerate t (vecAdd pa ma) s


vecAdd : Vec2 -> Vec2 -> Vec2
vecAdd a b = {x=a.x+b.x, y=a.y+b.y}


acceleration : Time -> Vec2 -> Mass -> Particle -> Vec2
acceleration t {x, y} attractorMass s =
  let
    dx  = s.pos.x - x
    dy  = s.pos.y - y
    rSq = (abs dx) + (abs dy)
    a   = -g * attractorMass / rSq
    r   = sqrt rSq
    ax  = a * dx / r
    ay  = a * dy / r
  in
    {x=ax, y=ay}

accelerate : Time -> Vec2 -> Particle -> Particle
accelerate t a s =
  let
    vx' = s.velocity.x + a.x*t
    x'  = s.pos.x + (s.velocity.x + vx')/2*t

    vy' = s.velocity.y + a.y*t
    y'  = s.pos.y + (s.velocity.y+vy')/2*t
  in
    { s | pos = {x=x', y=y'}
        , velocity = {x=vx', y=vy'}
    }



-- SUBSCRIPTIONS --

subscriptions : World -> Sub Message
subscriptions _ = Sub.batch [AnimationFrame.diffs Step, Window.resizes Resize ]



-- MAIN --

main : Program Never
main = App.program { init = init
                   , update = update
                   , view = view
                   , subscriptions = subscriptions
                   }
