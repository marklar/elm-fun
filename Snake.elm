{-
  "Snake" game.
  Eat the apples for points.
  As you eat, you grow (and speed up?).
  Avoid hitting the outer wall -- or yourself!
-}

import Keyboard
import Window

------------
-- INPUTS --
------------

type Direction = {x:Int, y:Int}

type Input =
    { space    : Bool        {- pause or go -}
    , arrowDir : Direction   {- direction of movement -}
    , delta    : Time        {- time transpired -}
    }

-- fps : number -> Signal Time  // seq of time deltas
-- inSeconds : Time -> Float
delta : Signal Time
delta = inSeconds <~ fps 10

-- sampleOn : Signal a -> Signal a -> Signal b
-- Sample from 2nd input every time an event occurs on the 1st.
-- In this case: every 'delta', take the Input.
input : Signal Input
input = sampleOn delta (Input <~ Keyboard.space
                               ~ Keyboard.arrows
                               ~ delta)

-----------
-- MODEL --
-----------

(gameWidth, gameHeight) = (600, 400)

type Positionable a = { a | x:Float, y:Float }
type Moving a = { a | vx:Float, vy:Float }
type Movable = Moving (Positionable {})

-- Rectangle.
-- If moving L/R, taller than wide.
-- If moving U/D, wider than tall.
type Segment = Movable

-- Snake: includes velocity?
--   When advancing: append to head, remove tail.
--   When eating: just append to head (without removing tail).
type Snake =
    { direction : Direction
    , body      : [Segment]
    }

type Apple = Positionable {}

data State = Play | Pause

type Game =
    { state : State
    , snake : Snake
    , apples : [Apple]
    , score : Int
    , caption : String
    }

newGame : Game
newGame =
    { state = Pause
    , snake = { direction = {x=0, y=1}
              , body = []
              }
    , apples = []
    , score = 0
    , caption = "None"
    }


-------------
-- Updates --
-------------

stepSnake : Input -> Snake -> Snake
stepSnake ({space, arrowDir, delta} as input) ({direction, body} as snake) =
    { snake | direction <- arrowDir
            , body <- body {- FIXME: Make list of body segments. -}
            }

dirToStr : Direction -> String
dirToStr {x,y} = "x:" ++ show x ++ ", y:" ++ show y

stepGame : Input -> Game -> Game
stepGame ({space, arrowDir, delta} as input) ({state, snake, apples, score, caption} as game) =
    { game | snake <- stepSnake input snake
           , caption <- dirToStr arrowDir
           -- , state <- getState state space
           }

gameState : Signal Game
gameState = foldp stepGame newGame input

-------------
-- Display --
-------------
{-
  The view is totally independent of how the game updates. It is based
  only on the model. This means we can change how the game looks
  without changing any of the update logic of the game.
-}

snakeGreen = rgb 60 100 60
gardenBrown = lightBrown

-- Show either this or nothing.
caption = "SPACE to start/pause.  &larr;&uarr;&darr;&rarr; to move."

showCaption : String -> Form
showCaption caption =
    move (0, -(gameHeight/2) + 20) (toForm (asText caption))

garden : Form
garden = rect gameWidth gameHeight |> filled gardenBrown

showMovable : Shape -> Movable -> Form
showMovable shape {x,y} = shape |> filled white |> move (x,y)

showSegment : Form
showSegment = rect 10 10 |> filled snakeGreen

display : (Int,Int) -> Game -> Element
display (w,h) {state, snake, apples, score, caption} =
    container w h middle <| collage gameWidth gameHeight
       [ garden
       , showSegment
       , showCaption caption
       ]
    
main : Signal Element
main = lift2 display Window.dimensions gameState
