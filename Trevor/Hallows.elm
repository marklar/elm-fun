main : Element
main = collage 500 500 [ symbolText |> scale 2
                                    |> rotate (degrees -6.9159095)
                                    |> move (80,50) ]

symbolText : Form
symbolText = group [ scale 2 symbol
                   , move (0,-60) (toForm textElement)
                   ]

textElement : Element
textElement = centered (toText "Deathly  Hallows")

triangle : Shape
triangle = ngon 3 45

symbol : Form
symbol = group [ rotate (degrees 90) (outlined lineStyle triangle) 
               , outlined lineStyle (oval 45 45)
               , traced lineStyle (segment (0,45) (0,-22.5))
               ]
                
lineStyle : LineStyle
lineStyle = solid blue
