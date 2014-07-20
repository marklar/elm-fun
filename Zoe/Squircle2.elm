import Keyboard

circle : Form
circle = filled blue (ngon 200 50)

square : Form
square = rotate (degrees 45) (filled red (ngon 4 100))

squircle : Form
squircle = group [square, circle]

(width, height) = (700, 700)

dist = 10

display : (Float,Float) -> Element
display pos =
      collage width height
          [ move pos squircle ]

getNewPosition : {x:Int,y:Int} -> (Float,Float) -> (Float,Float)
getNewPosition {x,y} (oldX, oldY) =
    ( oldX + (dist * toFloat x)
    , oldY + (dist * toFloat y) )

posSignal : Signal (Float,Float)
posSignal = foldp getNewPosition (0,0) Keyboard.arrows

main : Signal Element
main = display <~ posSignal
