import Mouse
import Keyboard

type Direction = {x:Int, y:Int}
type MousePosition = (Int,Int)

displaySignals : Direction -> MousePosition -> Element
displaySignals dir pos = asText (dir, pos)

ball : Form
ball = filled blue (oval 100 100)

collWd = 1000
collHt = 800

mouse2collageX : Float -> Float
mouse2collageX x = x - (collWd / 2)

mouse2collageY : Float -> Float
mouse2collageY y = collHt/2 - y

mouse2collage : MousePosition -> (Float,Float)
mouse2collage (x,y) =
    (mouse2collageX (toFloat x), mouse2collageY (toFloat y))

displayOld : Direction -> MousePosition -> Element
displayOld dir mousePos =
    collage collWd collHt
      [ move (0,200) (toForm (displaySignals dir mousePos))
      , move (mouse2collage mousePos) ball
      ]

display : MousePosition -> Element
display mousePos =
    let collPos = (mouse2collage mousePos)
    in collage collWd collHt
      [ move collPos ball
      , move (0,300) (toForm (asText ("mouse: " ++ show mousePos)))
      , move (0,250) (toForm (asText ("collage: " ++ show collPos)))
      ]

displayNew : (Float,Float) -> Element
displayNew ballPos =
    collage collWd collHt [ move ballPos ball ]

dir : Signal Direction
dir = Keyboard.arrows

pos : Signal MousePosition
pos = Mouse.position

-- `<~` : Transform a Signal with a given function.
-- Signal: something that can change over time.

-- main : Signal Element
-- main = displayOld <~ dir ~ pos

-- main : Signal Element
-- main = display <~ pos

moveFactor = 10

updateBallPosition : Direction -> (Float,Float) -> (Float,Float)
updateBallPosition {x,y} (px,py) =
    ( px + toFloat (moveFactor * x)
    , py + toFloat (moveFactor * y) )

ballPosition : Signal (Float,Float)
ballPosition = foldp updateBallPosition (0,0) dir

main : Signal Element
main = displayNew <~ ballPosition
