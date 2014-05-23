import Keyboard
import Random
import Window

------------
-- INPUTS --
------------

num : Signal Int
num = Random.range 0 100 Keyboard.space

delta : Signal Time
delta = inSeconds <~ fps 10

type Input = { space: Bool
             , num: Int
             , delta: Time   -- don't actually need this
             }

input = sampleOn delta (Input <~ Keyboard.space
                              ~ num
                              ~ delta)

-----------
-- MODEL --
-----------

-- type synonym
-- 'Positionable' is not a data constructor
type Positionable a = { a | x:Float, y:Float }

-- an actual type
data Tree a = Empty | Node a (Tree a) (Tree a)

leaf : a -> Tree a
leaf x = Node x Empty Empty

toSExp : Tree a -> String
toSExp t =
    case t of
      Empty -> "()"
      (Node v left right) ->
          join " " ["(" ++ show v, toSExp left, toSExp right ++ ")"]

-------------
-- Updates --
-------------

insertVal : comparable -> Tree comparable -> Tree comparable
insertVal x t =
    case t of
      Empty -> leaf x
      Node v left right ->
          if | x <= v    -> Node v (insertVal x left) right
             | otherwise -> Node v left (insertVal x right)

updateTree : Input -> Tree comparable -> Tree comparable
updateTree {space, num} tree =
    if space then insertVal num tree else tree

treeState : Signal (Tree comparable)
treeState = foldp updateTree Empty input

-------------
-- Display --
-------------

textColor = rgb 255 255 255
nodeColor = rgb 60 60 60

display : (Int,Int) -> Tree a -> Element
display (w,h) tree =
    container w h middle <| leftAligned (toText (toSExp tree))
    -- container w h middle <| leftAligned (toText (show tree))

main : Signal Element
main = lift2 display Window.dimensions treeState
