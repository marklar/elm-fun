-- Forked from http://elm-lang.org/edit/examples/Intermediate/Mario.elm

import Keyboard
import Window

------------
-- INPUTS --
------------

input : Signal (Time, Dir)
input =
    let
        -- fps : number -> Signal Time
        -- delta : Signal Time  (ms passed DIV BY 20??)
        delta = lift (\t -> t/20) (fps 25)
    in
        -- sampleOn : Signal a -> Signal b -> Signal b
        -- Sample from the second input every time an event occurs on the first input.
        -- Sequence actions discarding the first result.
        -- `a` here is Time
        sampleOn delta (lift2 (,) delta Keyboard.arrows)

-----------
-- MODEL --
-----------

-- x,y: pixels
-- velocity: pixels per timeslice
type Mario = { x:Float, y:Float, vx:Float, vy:Float, dir:String}
type Dir = { x:Int, y:Int }

mario : Mario
mario = { x=0, y=0, vx=0, vy=0, dir="right" }

------------
-- UPDATE --
------------

onGround : Mario -> Bool
onGround m = m.y == 0
-- onGround m = True

-- If up-arrow (y) is up, give Mario positive Y velocity.
jump : Dir -> Mario -> Mario
jump {y} m =
  if y > 0 && onGround m
  then { m | vy <- 5 }
  else m

-- Reduce Mario's Y velocity.
gravity : Time -> Mario -> Mario
gravity t m =
  if m.y > 0
  then { m | vy <- m.vy - t/4 }
  else m

-- Update Mario's X,Y position, based on velocity & time transpired.
physics : Time -> Mario -> Mario
physics t m =
        -- ms passed * X velocity.
  { m | x <- m.x + (t * m.vx)
        -- ms passed * Y velocity, bound by 0.
      , y <- max 0 (m.y + (t * m.vy)) }

-- (Misnomer: changes in x-velocity can happen in air, too.)
-- Based on right/left arrow (x)...
walk : Dir -> Mario -> Mario
walk {x} m =
      -- Update velocity.  (`physics` is responsible for changes in offset.)
  { m | vx <- toFloat x
      -- Update orientation.  (If arrow l/r, orient l/r.)
      , dir <- if | x < 0     -> "left"
                  | x > 0     -> "right"
                  | otherwise -> m.dir }

-- At each timestep (t: milliseconds)
-- (Point-free style.)
step : (Time, Dir) -> Mario -> Mario
step (t,dir) =
  physics t . walk dir . gravity t . jump dir

marioImgSrc : Mario -> String
marioImgSrc mario =
    let
        verb = if | mario.y  >  0 -> "jump"
                  | mario.vx /= 0 -> "walk"
                  | otherwise     -> "stand"
    in
        "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"

-------------
-- DISPLAY --
-------------

skyBlue : Color
skyBlue = rgb 174 238 238
grassGreen : Color
grassGreen = rgb 74 163 41

showSky : Float -> Float -> Form
showSky w h = rect w h |> filled skyBlue

showGround : Float -> Float -> Form
showGround w h =
    rect w 50 |> filled grassGreen
              |> move (0, 24 - h/2) 

showMario : Float -> Mario -> Form
showMario h mario =
    image 35 35 (marioImgSrc mario)
          |> toForm
          |> move (mario.x, mario.y + 62 - h/2)

-- window dimensions
render : (Int,Int) -> Mario -> Element
render (w,h) mario =
  let (w',h') = (toFloat w, toFloat h)
  in collage w h
      [ showSky w' h'
      , showGround w' h'
      , showMario h' mario
      ]

main : Signal Element
main  =
    -- render : (Int,Int) -> mario -> Signal Element
    -- Window.dimensions : Signal (Int,Int)
    lift2 render Window.dimensions (foldp step mario input)
