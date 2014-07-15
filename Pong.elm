-- Forked from here: http://elm-lang.org/blog/Pong.elm

import Keyboard
import Window

{-
  ToDo: 
    - upon restart, player who won point 'serves'
-}


------------
-- INPUTS --
------------

type Input = { space : Bool    -- If 'space' pressed
             , leftDir  : Int
             , rightDir : Int
             , delta : Time    -- Time transpired
             }

-- fps : number -> Signal Time   // seq of time deltas
-- inSeconds : Time -> Float
delta : Signal Time
delta = inSeconds <~ fps 35

-- sampleOn : Signal a -> Signal a -> Signal b
-- Sample from 2nd input every time an event occurs on the 1st.
-- In this case: every 'delta' take the Input.
input : Signal Input
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .y Keyboard.wasd
                               ~ lift .y Keyboard.arrows
                               ~ delta)

-----------
-- MODEL --
-----------

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

-- Setting these values like this...
-- (halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 200)
-- ...doesn't work.  It makes the UI jump around.
-- How can one set values just once and leave them?

type Positionable a = { a | x:Float, y:Float }
type Moving a = { a | vx:Float, vy:Float }
type Movable = Moving (Positionable {})

{-
   We could add dimensions attributes:
     Ball: radius
     Paddle: width, height
   However, these never change throughout the game.
   Perhaps it makes sense to make part of the model
   only those things which might be updated?
-}
type Ball = Movable
type Paddle = Movable

type Player = { paddle:Paddle, score:Int }

-- GameState
-- Is this the state of the Game, or of the Ball?
data State = Play | Pause

type Game = { state : State
            , ball : Ball
            , leftPlayer : Player
            , rightPlayer : Player
            }

-- Parametrize X offset of player.
makePlayer : Float -> Player
makePlayer x = { paddle = {x=x, y=0, vx=0, vy=0}
               , score = 0 }

-- How each game starts.
newGame : Game
newGame =
  { state   = Pause
  , ball    = { x=0, y=0, vx=200, vy=200 }
  , leftPlayer  = makePlayer (20 - halfWidth)
  , rightPlayer = makePlayer (halfWidth - 20)
  } 


-------------
-- Updates --
-------------

-- Are 'a' and 'b' within 'thresh' each other?
near : Float -> Float -> Float -> Bool
near a thresh b =
    (abs (a - b)) <= thresh

data Contact = LeftPaddle | RightPaddle | NoPaddle

hit : Paddle -> Ball -> Bool
hit paddle ball =
    (ball.x |> near paddle.x 8) && (ball.y |> near paddle.y 20)

getHitPaddle : Ball -> Paddle -> Paddle -> Maybe Paddle
getHitPaddle ball left right =
    if | hit left ball -> Just left
       | hit right ball -> Just right
       | otherwise -> Nothing

-- new velocity for Ball
maybeBounce : Float -> Bool -> Bool -> Float
maybeBounce v hitMin hitMax =
  if | hitMin    -> abs v       -- switch to positive
     | hitMax    -> 0 - abs v   -- switch to negative
     | otherwise -> v

offCourt : Ball -> Bool
offCourt ball = not (ball.x |> near 0 halfWidth)

placeInCenter : Ball -> Ball
placeInCenter ball =
    let vy = 100
    in { ball | x <- 0
              , y <- 0
              , vx <- getVx ball.vx vy 400
              , vy <- vy }

stepObj : Time -> Movable -> Movable
stepObj t obj =
    { obj | x <- obj.x + obj.vx * t
          , y <- obj.y + obj.vy * t }

getVy : Float -> Maybe Paddle -> Float
getVy vy hitPaddle =
    let vyDelta = case hitPaddle of 
                    Just p -> p.vy * 0.25
                    Nothing -> 0
    in
      clamp -350 350 (vy + vyDelta)

getVx : Float -> Float -> Float -> Float
getVx vx vy total =
    let magnitude = sqrt (total^2 - vy^2)
    in if vx >=0 then magnitude else 0 - magnitude

getVxVy : Float -> Float -> Maybe Paddle -> (Float,Float)
getVxVy vx vy hitPaddle =
    let vy' = getVy vy hitPaddle
        vx' = getVx vx vy' 400
        vx'' = case hitPaddle of
                 Just p -> 0 - vx'   -- reverse!
                 Nothing -> vx'
    in (vx'', vy')

stepBall : State -> Time -> Paddle -> Paddle -> Ball -> Ball
stepBall state t leftPaddle rightPaddle ({x,y,vx,vy} as ball) =
  if | state == Pause -> ball
     | offCourt ball  -> placeInCenter ball
     | otherwise ->
         let hitPaddle = getHitPaddle ball leftPaddle rightPaddle
             hitBtm = (y < 7-halfHeight)
             hitTop = (y > halfHeight-7)
             (newVx, newVy) = getVxVy vx vy hitPaddle
         in
           stepObj t { ball | vx <- newVx
                            , vy <- maybeBounce newVy hitBtm hitTop }

stepPaddle : Time -> Int -> Paddle -> Paddle
stepPaddle t dir paddle =
    let
        -- set its Y velocity
        p = { paddle | vy <- toFloat dir * 300 }
        -- then move it
        p' = stepObj t p
    in
      -- then bound its y movement
      { p' | y <- clamp (22-halfHeight) (halfHeight-22) p'.y }

stepPlayer : Time -> Int -> Maybe Player -> Player -> Player
stepPlayer t dir scorer ({paddle,score} as player) =
    let scoreInc = case scorer of
                     Nothing -> 0
                     Just p -> if p == player then 1 else 0
    in
      { player | paddle <- stepPaddle t dir paddle
               , score <- player.score + scoreInc }

getScorer : Game -> Maybe Player
getScorer {ball,leftPlayer,rightPlayer} =
    if | ball.x > halfWidth  -> Just leftPlayer
       | ball.x < -halfWidth -> Just rightPlayer
       | otherwise           -> Nothing

getState : State -> Bool -> Maybe Player -> State
getState state space scorer =
    -- if | space -> if (state == Play) then Pause else Play
    if | space             -> Play
       | scorer /= Nothing -> Pause
       | otherwise         -> state

stepGame : Input -> Game -> Game
stepGame {space,leftDir,rightDir,delta} ({state,ball,leftPlayer,rightPlayer} as game) =
  let scorer = getScorer game
  in
    { game | state   <- getState state space scorer
           -- Do NOT use new state in stepBall.
           -- Ball won't move far enough to induce Pause.
           , ball    <- stepBall state delta leftPlayer.paddle rightPlayer.paddle ball 
           , leftPlayer  <- stepPlayer delta leftDir scorer leftPlayer
           , rightPlayer <- stepPlayer delta rightDir scorer rightPlayer }

gameState : Signal Game
gameState = foldp stepGame newGame input


-------------
-- Display --
-------------
{-
  The view is totally independent of how the game updates. It is based
  only on the model. This means we can change how the game looks
  without changing any of the update logic of the game.
-}

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160

caption = "SPACE to start, WS and &uarr;&darr; to move"

txt : (Text -> Text) -> String -> Element
txt f = leftAligned . f . monospace . Text.color textGreen . toText

court : Form
court = rect gameWidth gameHeight |> filled pongGreen

showMovable : Shape -> Movable -> Form
showMovable shape {x,y} = shape |> filled white |> move (x,y)

showBall : Ball -> Form
showBall = showMovable (oval 15 15)

showPaddle : Paddle -> Form
showPaddle = showMovable (rect 10 40)

showScore : (Int,Int) -> Form
showScore (left, right) =
    let elem = txt (Text.height 50) (show left ++ "  " ++ show right)
    in toForm elem |> move (0, gameHeight/2 - 40)

showCaption : State -> String -> Form
showCaption state str =
    toForm (if state == Play then spacer 1 1 else txt id str)
        |> move (0, 40 - gameHeight/2)

display : (Int,Int) -> Game -> Element
display (w,h) {state,ball,leftPlayer,rightPlayer} =
    -- collage : Int -> Int -> [Form] -> Element
    container w h middle <| collage gameWidth gameHeight
                  [ court
                  , showBall ball
                  , showPaddle leftPlayer.paddle
                  , showPaddle rightPlayer.paddle
                  , showScore (leftPlayer.score, rightPlayer.score)
                  , showCaption state caption
                  ]

main : Signal Element
main = lift2 display Window.dimensions gameState
