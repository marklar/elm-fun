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

displayDir : {x:Int, y:Int} -> Element
displayDir arrowDir =
    collage collWd collHt [ toForm (asText arrowDir) ]

displayAll : (Float,Float) -> {x:Int, y:Int} -> Element
displayAll ballPos arrowDir =
    collage collWd collHt
        [ move ballPos ball
        , move (0, collHt/2 - 10) (toForm (asText arrowDir))
        ]

-- main : Element
-- main = displayBall (100,100)

main : Signal Element
main = displayBall <~ ballPosition
-- main = displayDir <~ arrowDir
-- main = displayAll <~ ballPosition ~ arrowDir

{-

1. Display a non-moving ball in collage.  Use main : Element
2. Move it around with `move`.  No input.
3. Introduce concept of Signal.
4. Display the arrow direction in collage.
5. Display ball again, unmoving.
6. Introduce notion of 'update'.  Need to remember where it was.
7. foldp : Uses a function to take a starting value and a Signal
           and produces a new Signal (of same type as starting value).
8. Use ballPosition Signal as input to display function.  Move ball around.

-}