import Keyboard
import Array
import Random

collWd = 600
collHt = 600

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}
data Light = Green | Red

-----------
-- MODEL --

elapsedSignal : Signal Time
elapsedSignal = inSeconds <~ fps 35

appleSignal : Signal Pos
appleSignal = 
    let f a b = (toFloat a, toFloat b)
    in lift2 f (Random.range 10 (collWd - 10) (every second))
               (Random.range 10 (collHt - 10) (every second))


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

snakeGreen : Color
snakeGreen = rgb 40 140 80

segment : Form
segment = filled snakeGreen (square 20)

extreme : Form
extreme = filled snakeGreen (circle 10)

showSnake : Snake -> [Form]
showSnake {hd,front,back,tl} =
    let drawBody pos = move pos segment
    in move hd extreme ::
       move tl extreme ::
       (map drawBody front) ++ (map drawBody back)

display : State -> Input -> Pos -> Element
display ({snake} as state) input apple =
    flow down [ asText input
              , asText apple
              , collage collWd collHt (showSnake snake)
              ]

main : Signal Element
main = display <~ stateSignal ~ inputSignal ~ appleSignal
