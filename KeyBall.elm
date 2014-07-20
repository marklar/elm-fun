import Keyboard

-------------
-- SIGNALS --
-------------

arrowDir : Signal {x:Int, y:Int}
arrowDir = Keyboard.arrows

ballPosition : Signal (Float,Float)
ballPosition = foldp updateBallPosition (0,0) arrowDir

------------
-- UPDATE --
------------

moveFactor = 10

updateBallPosition : {x:Int, y:Int} -> (Float,Float) -> (Float,Float)
updateBallPosition {x,y} (px,py) =
    ( px + toFloat (moveFactor * x)
    , py + toFloat (moveFactor * y) )

-------------
-- DISPLAY --
-------------

ball : Form
ball = filled blue (oval 100 100)

collWd = 500
collHt = 400

displayBall : (Float,Float) -> Element
displayBall ballPos =
    collage collWd collHt [ move ballPos ball ]

main : Signal Element
main = displayBall <~ ballPosition
