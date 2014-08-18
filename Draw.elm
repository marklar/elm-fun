import Mouse

-----------
-- Config
-----------

lineStyle = { defaultLine | width <- 8
                          , color <- blue }
collWd = 1000
collHt = 700

----------
-- Types
----------

type MousePos = (Int,Int)
type CollagePos = (Float,Float)

---------------------
-- Input Conversion
---------------------

mouse2collageX : Float -> Float
mouse2collageX x = x - (collWd / 2)

mouse2collageY : Float -> Float
mouse2collageY y = (collHt / 2) - y

mouse2collage : MousePos -> CollagePos
mouse2collage (x,y) =
    ( mouse2collageX (toFloat x)
    , mouse2collageY (toFloat y)
    )

----------
-- MODEL 
----------

type State = [CollagePos]
newState = []

-----------
-- UPDATE
-----------

updateState : MousePos -> State -> State
updateState mousePos state =
    (mouse2collage mousePos) :: state

----------
-- INPUT
----------

stateSignal : Signal State
stateSignal = foldp updateState newState Mouse.position

------------
-- DISPLAY
------------

drawLine : State -> Form
drawLine state =
    traced lineStyle (path state)

display : State -> Element
display state =
    collage collWd collHt [ drawLine state ]


main : Signal Element
main = display <~ stateSignal
