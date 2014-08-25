import Keyboard
import Window

type ArrowDir = {x:Int, y:Int}
type Ball = {y:Float, vy:Float }

ball : Ball
ball = { y=0, vy=0 }

-- UPDATE

springiness = 0.5
gravity = 3.0

isUpArrow : ArrowDir -> Bool
isUpArrow arrowDir = arrowDir.y > 0

isOnGround : Ball -> Bool
isOnGround ball = ball.y <= 0

pushUp : Ball -> Ball
pushUp ball = { ball | vy <- 50 }

accelerateDown : Float -> Ball -> Ball
accelerateDown timeDelta ball =
  { ball | vy <- ball.vy - (timeDelta * gravity) }

bounce : Float -> Ball -> Ball
bounce timeDelta ball =
  { ball | vy <- (abs ball.vy) - (timeDelta * 4.0 / springiness) }

travel : Float -> Ball -> Ball
travel timeDelta ball =
  { ball | y <- max 0 (ball.y + timeDelta * ball.vy) }

calcVelocity : (Float,ArrowDir) -> Ball -> Ball
calcVelocity (timeDelta, arrowDir) ball =
  if isOnGround ball
  then if isUpArrow arrowDir
       then pushUp ball
       else bounce timeDelta ball
  else accelerateDown timeDelta ball
    
step : (Float,ArrowDir) -> Ball -> Ball
step (timeDelta, arrowDir) ball =
  calcVelocity (timeDelta, arrowDir) (travel timeDelta ball)

-- DISPLAY

render : (Int,Int) -> Ball -> Element
render (w',h') ball =
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h'
      [ rect w h  |> filled grassColor
      , rect w 50 |> filled skyColor |> move (0, 24 - h/2)
      , circ |> move (0, ball.y + 72 - h/2)
      ]

circ : Form
circ = filled ballColor (circle 25)

ballColor = darkBlue
grassColor = rgb 174 238 238
skyColor = rgb 74 163 60

-- 60 times per second,
-- input is 
input : Signal (Float, ArrowDir)
input = let delta = (\t -> t / 20.0) <~ (fps 60)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main : Signal Element
main = render <~ Window.dimensions ~ (foldp step ball input)
