import Keyboard
 
{-
circle : Form
circle = filled blue (ngon 200 50)

square : Form
square = rotate (degrees 45) (filled red (ngon 4 100))

squircle : Form
squircle = group [square, circle]
-}

(width, height) = (500, 500)

-- arrowSignal : Signal {x:Int, y:Int}
-- arrowSignal = Keyboard.arrows

display : {x:Int, y:Int} -> Element
display arrowsValue =
    collage width height
        [ toForm (asText arrowsValue) ]

main : Signal Element
main = display <~ Keyboard.arrows
