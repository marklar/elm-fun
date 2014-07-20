{- Baba is suuuuuuuuuper weird
plus he doesnt know what hes saying
-}

-- Symbol --

clr : Color
clr = black

tri : Form
tri = rotate (degrees 90) (outlined (solid clr) (ngon 3 100))

circlee : Form
circlee = outlined (solid clr) (circle 50)

line : Form
line = traced (solid clr) (segment (0,-50) (0,100))

dhSymbol : Form
dhSymbol = group [tri, circlee, line]


-- Text --

dhTextElem : Element
dhTextElem = centered (toText "Deathly Hallows")

dhTextForm : Form
dhTextForm = move (0,-70) (toForm dhTextElem)


-- The Whole Thing --

deathlyHallows : Form
deathlyHallows = group [dhSymbol, dhTextForm]


-- Display --

main : Element
main = collage 500 500 [ move (0,-20) deathlyHallows ]
