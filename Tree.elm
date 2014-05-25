import Keyboard
import Random
import Window
import Graphics.Input as Input

------------
-- INPUTS --
------------

buttonClicks : Input.Input Int
buttonClicks = Input.input 0

myButton : Element
myButton = Input.button buttonClicks.handle 1 "click me"

-----------
-- MODEL --
-----------

-- type synonym
-- 'Positionable' is not a data constructor
type Positionable a = { a | x:Float, y:Float }

-- an actual type
data Tree a = Empty | Node a (Tree a) (Tree a)

type State a = { tree: Tree a
               , lastNum: Maybe Int
               }

initState : State Int
initState = { tree = Empty
            , lastNum = Nothing
            }

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

-- Takes one Tree and inserts it into another.
-- (Useful for deleting values from a tree.)
insertTree : Tree comparable -> Tree comparable -> Tree comparable
insertTree ta tb =
    case (ta, tb) of
      (t, Empty) -> t
      (Empty, t) -> t
      (Node a _ _, Node b left right) ->
          if | a <= b    -> Node b (insertTree ta left) right
             | otherwise -> Node b left (insertTree ta right)

-- If not found, just return the same tree.
delete : comparable -> Tree comparable -> Tree comparable
delete x t =
    case t of
      Empty             -> Empty
      Node v left right ->
          if | x > v     -> Node v left (delete x right)
             | x < v     -> Node v (delete x left) right
             | otherwise -> insertTree left right  -- Promotes right.

-- f's args: nodeVal leftResult rightResult
foldTree : (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f acc t =
    case t of
      Empty             -> acc
      Node v left right -> f v (foldTree f acc left) (foldTree f acc right)

-- Highest val: always on far right.
tMax : Tree a -> Maybe a
tMax =
    let g v _ rRes = case rRes of Nothing -> Just v
                                  otherwise -> rRes
    in foldTree g Nothing

-- Lowest val: always on far left.
tMin : Tree a -> Maybe a
tMin =
    let g v lRes _ = case lRes of Nothing   -> Just v
                                  otherwise -> lRes
    in foldTree g Nothing

-- Number of values.
tSize : Tree a -> Int
tSize = foldTree (\_ lRes rRes -> 1 + lRes + rRes) 0

-- How many levels in the tree?
tDepth : Tree a -> Int
tDepth = foldTree (\_ lRes rRes -> 1 + max lRes rRes) 0

-- Not ordered.
toList : Tree a -> [a]
toList = foldTree (\v lRes rRes -> v :: lRes ++ rRes) []

indentStrs : Tree a -> [String]
indentStrs =
    let g v ls (r::rs) = concat [ ["--" ++ show v]
                                , map ((++) "  |") ls
                                , ["  `" ++ r]
                                , map ((++) "   ") rs
                                ]
    in foldTree g ["-- /-"]

updateState : Int -> State Int -> State Int
updateState num ({tree, lastNum} as state) =
    { state | tree <- insertVal num tree
            , lastNum <- Just num }

randomInt : Signal Int
-- randomInt = Random.range 1 100 entered
-- randomInt = Random.range 1 100 (sampleOn buttonClicks.signal (constant 1))
randomInt = Random.range 1 100 (count buttonClicks.signal)
 -- buttonClicks.signal 

currentState : Signal (State Int)
currentState = foldp updateState initState randomInt

{-| Signal that updates when the enter key is pressed. We will use it to sample
other signals. Actual value of this signal is not important.
-}
entered : Signal ()
-- keep events that satisfy the given predicate
entered = always () <~ keepIf id True Keyboard.enter

-------------
-- Display --
-------------

textColor = rgb 255 255 255
nodeColor = rgb 60 60 60

showInfo : Tree a -> Maybe Int -> Element
showInfo t lastNum =
    let -- foo : String -> ((Tree a) -> b) -> Element
        foo s f = leftAligned (toText (s ++ show (f t)))
    in flow down [ leftAligned (toText ("last: " ++ show lastNum))
                 , foo "min: "  tMin
                 , foo "max: "  tMax
                 , foo "size: " tSize
                 , foo "depth: " tDepth
                 , foo "list: " toList
                 ]

showTree : Int -> Int -> Tree a -> Element
showTree w h tree =
    container w h middle <|
              flow down (map (leftAligned . toText) (indentStrs tree))
    -- container w h middle <| leftAligned (toText (show tree))
    -- container w h middle <| leftAligned (toText (toSExp tree))

display : (Int,Int) -> State Int -> Element
display (w,h) {tree, lastNum} =
    container w h middle <| collage w h
              [ move (-300, 100) <| toForm myButton
              , toForm (showTree w h tree)
              , move (-300, 10) <| toForm (showInfo tree lastNum)
              ]

main : Signal Element
main = lift2 display Window.dimensions currentState
