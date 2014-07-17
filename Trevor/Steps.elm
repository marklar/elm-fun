-- (if 1 == 1
--                 then "The numbers are the same."
--                 else "The numbers are different."
--              )

whatToDo : Int -> String
whatToDo n = 
  if n == 4 then "turn left" else "keep going"


main = asText [ (whatToDo 1)
              , (whatToDo 2)
              , (whatToDo 3)
              , (whatToDo 4)
              ]

{--
  [ (if 1 == 4 then "turn left" else "keep going")
  , (if 2 == 4 then "turn left" else "keep going")
  , (if 3 == 4 then "turn left" else "keep going")
  , (if 4 == 4 then "turn left" else "keep going")
  ]
--}  
