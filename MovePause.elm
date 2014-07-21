import Keyboard

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}

-----------
-- MODEL --

type Input = { space : Bool    -- (Un-)pause?
             , arrowDir : Dir  -- How to move *next*.
             , elapsed : Time  -- How long since last updated?
             }

inputSignal : Signal Input
inputSignal = lift3 (\s d t -> {space=s, arrowDir=d, elapsed=t})
              Keyboard.space
              Keyboard.arrows
              (fps 30)

data Light = Green | Red

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

-- Goes only when you tell it.  Can go diagonally.
getMustHoldDir : Dir -> Dir -> Dir
getMustHoldDir arrowDir ballDir = arrowDir

-- Always moving, either vertically or horizontally.
-- Changes direction when you tell it to.
getContinuousDir : Dir -> Dir -> Dir
getContinuousDir arrowDir ballDir =
    case (arrowDir.x, arrowDir.y) of
      (0,0) -> ballDir
      (0,y) -> { x=0, y=y }
      (x,_) -> { x=x, y=0 }

getBallDir : Dir -> Dir -> Dir
getBallDir = getContinuousDir

ballSpeed = 250.0

distance : Time -> Int -> Float
distance elapsed dirVal =
    ballSpeed * (inSeconds elapsed) * (toFloat dirVal)

getBallPos : Time -> State -> Pos
getBallPos elapsed {pos,ballDir,light} =
    case (light, pos) of
      (Red, _) -> pos
      (Green, (x,y)) ->
          (x + (distance elapsed ballDir.x),
           y + (distance elapsed ballDir.y))

getLight : Bool -> Light -> Light
getLight space light =
    case (space, light) of
      (False, light) -> light
      (True, Red)    -> Green
      (True, Green)  -> Red

updateState : Input -> State -> State
updateState {space,arrowDir,elapsed} state =
    { pos = getBallPos elapsed state
    , ballDir = getBallDir arrowDir state.ballDir
    , light = getLight space state.light
    }

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
