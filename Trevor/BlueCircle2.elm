import Keyboard

myCircle : Float -> Shape
myCircle radius = oval (2*radius) (2*radius)

blueCircle : Form
blueCircle = outlined (dashed blue) (myCircle 45)

display : (Float,Float) -> Element
display pos = 
    collage 250 300 [ move pos blueCircle ]

newPosition : {x:Int,y:Int} -> (Float,Float) -> (Float,Float)
newPosition arrows (prevX,prevY) =
    ( prevX + (10 * (toFloat arrows.x))
    , prevY + (10 * (toFloat arrows.y)) )

posSignal : Signal (Float,Float)
posSignal = foldp newPosition (0,0) Keyboard.arrows

main : Signal Element
main = display <~ posSignal
