import Keyboard

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}
data Light = Green | Red

-----------
-- MODEL --

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

elapsedSignal : Signal Time
elapsedSignal = inSeconds <~ fps 35

inputSignal : Signal Input
inputSignal = Input <~ (dropRepeats lightSignal)
                     ~ (dropRepeats Keyboard.arrows)
                     ~ elapsedSignal

type State = { pos : Pos
             , ballDir : Dir
             , light : Light
             }

initState : State
initState = { pos = (0,0)
            , ballDir = {x=0, y=1}
            , light = Red
            }

stateSignal : Signal State
stateSignal = foldp updateState initState inputSignal
              
------------
-- UPDATE --

updateLightInput : Bool -> LightInput -> LightInput
updateLightInput newSpace {space,light} =
    { space = newSpace
    , light = case (newSpace, space, light) of
                (True, False, Red)   -> Green
                (True, False, Green) -> Red
                _                    -> light
    }

turn : Int -> Int -> Int
turn arrowVal ballVal =
    if ballVal == 0 then arrowVal else ballVal

-- Always move, either vertically or horizontally.
-- Change direction when you tell it to.
-- May not reverse direction.  Only turn.
-- TODO: Use only l/r keys?
getBallDir : Dir -> Dir -> Dir
getBallDir arrowDir ballDir =
    case (arrowDir.x, arrowDir.y) of
      (0,0) -> ballDir
      (0,y) -> { x = 0, y = turn y ballDir.y }
      (x,_) -> { y = 0, x = turn x ballDir.x }

ballSpeed = 250.0

distance : Time -> Int -> Float
distance elapsed dirVal =
    ballSpeed * elapsed * (toFloat dirVal)

getBallPos : Time -> State -> Pos
getBallPos elapsed {pos,ballDir} =
    let (x,y) = pos
    in  (x + (distance elapsed ballDir.x),
         y + (distance elapsed ballDir.y))

updateState : Input -> State -> State
updateState {light,arrowDir,elapsed} state =
    if state.light == Green
      then { pos = getBallPos elapsed state
           , ballDir = getBallDir arrowDir state.ballDir
           , light = light
           }
      else { state | light <- light }

-------------
-- DISPLAY --

ball : Form
ball = filled blue (circle 30)

collWd = 600
collHt = 600

display : State -> Input -> Element
display ({pos} as state) input =
    flow down [ asText input
              , asText state
              , collage collWd collHt [ move pos ball ]
              ]

main : Signal Element
main = display <~ stateSignal ~ inputSignal
