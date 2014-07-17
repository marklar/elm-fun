import Keyboard

{-
myCircle : Float -> Shape
myCircle radius = oval (2*radius) (2*radius)

blueCircle : Form
blueCircle = outlined (dashed blue) (myCircle 45)
-}

display : {x:Int, y:Int} -> Element
display arrows = 
    -- asText arrows
    collage 500 600 [ toForm (asText arrows) ]

main : Signal Element
main = display <~ Keyboard.arrows
