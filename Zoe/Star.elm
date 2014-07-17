tri = outlined (dotted lightPurple) (ngon 3 100.0)

rotTri numDegrees =
    rotate (degrees numDegrees) (scale 1.5 tri)

ourStyle = { defaultStyle | color <- blue }

zoeText = style ourStyle (toText "Zoe")

zoeStar = group
    [ rotTri 90
    , rotTri 30
    , rotate (degrees 180) (scale 2.0 (toForm (centered zoeText)))
    ]

main = collage 600 600
  [ move (-15,-10) zoeStar
  , move (0,0) zoeStar
  ]
