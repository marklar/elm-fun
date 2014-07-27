import Keyboard
import Array
import Random

collWd : Int
collWd = 1400
collHt : Int
collHt = 750

boundCoords : Int -> (Int,Int)
boundCoords full =
    let half = round (toFloat full / 2)
    in ((10 - half), (half - 10))

-- (Int,Int)
(minX, maxX) = boundCoords collWd
(minY, maxY) = boundCoords collHt

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}
data Light = Green | Red

-----------
-- MODEL --

-- We want the apple to remain where it is until eaten.
-- Once eaten, we want a new one to appear.
-- We need it to appear where it's not in contact with any part of the snake.

-- How do we make separate modules?


elapsedSignal : Signal Time
elapsedSignal = inSeconds <~ fps 35

appleSignal : Signal Pos
appleSignal = 
    let f a b = (toFloat a, toFloat b)
    in lift2 f (Random.range minY maxX (every (3000 * millisecond)))
               (Random.range minY maxY (every (3000 * millisecond)))


type Input = { light : Light
             , arrowDir : Dir  -- How to move *next*.
             , elapsed : Time  -- How long since last updated?
             }

type LightInput = { space : Bool
                  , light : Light
                  }

lightSignal : Signal Light
lightSignal = lift .light <|
                foldp updateLightInput (LightInput False Red) Keyboard.space

inputSignal : Signal Input
inputSignal = Input <~ (dropRepeats lightSignal)
                     ~ (dropRepeats Keyboard.arrows)
                     ~ elapsedSignal

type Snake = { hd : Pos
             , front : [Pos]
             , back : [Pos]
             , tl : Pos
             }

type State = { snakeDir : Dir
             , snake : Snake
             , light : Light
             }

{-

TODO: Rather than storing each position,
store only where the vertices (head, tail, and turns) are.

One way to do that is to record (Pos, Dir).
That is:
  1. where the body part is at that moment, and
  2. what direction it came from to get there.

If pushing a new value:
  * whose Dir is different from the previous, simply push.
    You have a new vertex.
  * whose Dir is the same as the previous,
    then you can simply replace the previous with the new one
    (thus extending that segment of the Path).

When removing a value, move it closer by the appropriate amount
to the subsequent.  If doing so makes its Pos equal to or *pass*
the other, then simply remove it.  To see whether it goes past,
look at the subsequent's Dir, and see whether the sign of the
delta in that direction changes.

-}

initSnake : Int -> Snake
initSnake numSegments =
    let init n = (0.0, 0.0 - ((toFloat n) * 8))
        segmentsAry = Array.initialize numSegments init
        len = Array.length segmentsAry
    in { hd    = Array.getOrElse (0,0) 0 segmentsAry
       , front = []
       , back  = reverse (Array.toList (Array.slice 1 (len-1) segmentsAry))
       , tl    = Array.getOrElse (0,0) (len-1) segmentsAry
       }

{-

How do we determine whether the head runs into something?
If the snake is a Path (list of line segments),
check it against each line segment.
If the head's Pos is within (2 * lineWidth) of the segment,
then it's touching.

First, calculate which point on the line (including the vertices)
is closest to the head vertex.

If the two vertices have the same Y value, then line is horizontal.
If the head's X value is between them, then distance = Y delta.
If not, distance = hypotenuse between the head and the closer vertex.
(Closer: smaller X delta.)

If the two vertices have the same X value, the line is vertical.
If the head's Y value is between them, then distance = X delta.
If not, distance = hypotenuse between the head and the closer vertex.
(Closer: smaller Y delta.)

-}

initState : State
initState = { snakeDir = {x=0, y=1}
            , snake = initSnake 20
            , light = Red
            }

stateSignal : Signal State
stateSignal = foldp updateState initState inputSignal
              
------------
-- UPDATE --

-- SNAKE

pushSnake : Pos -> Snake -> Snake
pushSnake newHeadPos snake =
    { snake | hd    <- newHeadPos
            , front <- snake.hd :: snake.front
            }

popSnake : Snake -> Snake
popSnake ({hd,front,back,tl} as snake) =
    case (front,back) of
      ([], []) -> snake
      (_, [])  -> let revFront = reverse front
                  in  { snake | front <- []
                              , back  <- tail revFront
                              , tl    <- head revFront
                              }
      otherwise -> { snake | back <- tail back
                           , tl   <- head back
                           }

moveSnake : Pos -> Snake -> Snake
moveSnake newHeadPos snake =
    snake |> pushSnake newHeadPos
          |> popSnake

-- LIGHT      

updateLightInput : Bool -> LightInput -> LightInput
updateLightInput newSpace {space,light} =
    { space = newSpace
    , light = case (newSpace, space, light) of
                (True, False, Red)   -> Green
                (True, False, Green) -> Red
                _                    -> light
    }

turn : Int -> Int -> Int
turn arrowVal snakeVal =
    if snakeVal == 0 then arrowVal else snakeVal

-- May not reverse direction.  Only turn.
-- TODO: Use only l/r keys?
getSnakeDir : Dir -> Dir -> Dir
getSnakeDir arrowDir snakeDir =
    case (arrowDir.x, arrowDir.y) of
      (0,0) -> snakeDir
      (0,y) -> { x = 0, y = turn y snakeDir.y }
      (x,_) -> { y = 0, x = turn x snakeDir.x }

snakeSpeed = 250.0

distance : Time -> Int -> Float
distance elapsed dirVal =
    snakeSpeed * elapsed * (toFloat dirVal)

getSnakePos : Time -> State -> Pos
getSnakePos elapsed {snake,snakeDir} =
    let (x,y) = snake.hd
    in  (x + (distance elapsed snakeDir.x),
         y + (distance elapsed snakeDir.y))

updateState : Input -> State -> State
updateState {light,arrowDir,elapsed} state =
    if state.light == Green
      then
          let newHeadPos = getSnakePos elapsed state
          in { snakeDir = getSnakeDir arrowDir state.snakeDir
             , snake = moveSnake newHeadPos state.snake
             -- , snake = pushSnake newHeadPos state.snake
             , light = light
             }
      else { state | light <- light }

-------------
-- DISPLAY --

--  (croppedImage (40,100) 40 80 "trev.png")
--  (croppedImage (80,60) 130 150 "zoe.png")

snakeGreen : Color
snakeGreen = rgb 40 140 80

showSnake : Snake -> Form
showSnake {hd,front,back,tl} =
    traced { defaultLine | color <- snakeGreen
                           , width <- 15
                           , cap   <- Round
                           , join  <- Smooth
                           }
             (path (hd :: front ++ (reverse back) ++ [tl]))

showApple : Pos -> Form
showApple applePos =
    move applePos (filled red (circle 10))

display : State -> Input -> Pos -> Element
display ({snake} as state) input apple =
    flow down [ asText input
              , asText apple
              , collage collWd collHt [ showSnake snake, showApple apple ]
              ]

main : Signal Element
main = display <~ stateSignal ~ inputSignal ~ appleSignal
