import Mouse

type MousePos = (Int,Int)
type CollagePos = (Float,Float)

main : Signal Element
main = display <~ positionsSignal

(collWd, collHt) = (800, 1000)

display : [CollagePos] -> Element
display positions =
  collage collWd collHt (map drawDot positions)

drawDot : CollagePos -> Form
drawDot pos = move pos (filled blue (circle 5))

positionsSignal : Signal [CollagePos]
positionsSignal = foldp (::) [ ] posSignal

posSignal : Signal CollagePos
posSignal = mouse2collage <~ mousePosSignal

mouse2collage : MousePos -> CollagePos
mouse2collage (x,y) =
  ( (toFloat x) - (collWd / 2)
  , (collHt / 2) - (toFloat y)
  )

mousePosSignal : Signal MousePos
mousePosSignal =
    merges [ sampleOn Mouse.clicks Mouse.position
           , keepWhen Mouse.isDown (0,0) Mouse.position
           ]
