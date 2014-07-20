import Keyboard

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}

-----------
-- MODEL --

type Ball = {pos:Pos, ballDir:Dir}

initBall : Ball
initBall = {pos = (0,0), ballDir = {x=0, y=1}}

-------------
-- SIGNALS --

type DeltaArrowDir = {delta:Time, arrowDir:Dir}

-- pixels per second
ballSpeed = 100.0

distance : Time -> Int -> Float
distance delta dirVal =
    ballSpeed * (inSeconds delta) * (toFloat dirVal)

getBallDir : Dir -> Dir -> Dir
getBallDir arrowDir ballDir =
    arrowDir
    -- Don't use both u/d and r/l.  If old val...

getBallPos : Time -> Ball -> Pos
getBallPos delta {pos,ballDir} =
    let (x,y) = pos
    in  ((x + (distance delta ballDir.x)),
         (y + (distance delta ballDir.y)))

updateBall : DeltaArrowDir -> Ball -> Ball
updateBall {delta,arrowDir} ball =
    { pos = getBallPos delta ball
    , ballDir = getBallDir arrowDir ball.ballDir
    }

ballSignal : Signal Ball
ballSignal = foldp updateBall initBall deltaArrowDir

-- How long since we last updated?  (Move some distance in *prev* dir.)
-- What direction should we move *next*?
deltaArrowDir : Signal DeltaArrowDir
deltaArrowDir = lift2 (\t d -> {delta=t, arrowDir=d})
                      (fps 20)
                      Keyboard.arrows

------------
-- DISPLAY --

ball : Form
ball = filled blue (circle 50)

collWd = 500
collHt = 400

displayBall : Ball -> Element
displayBall {pos} =
    collage collWd collHt [ move pos ball ]

main : Signal Element
main = displayBall <~ ballSignal
