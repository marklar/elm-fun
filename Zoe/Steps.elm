-- myValue = "Hi, my name is Zoe"
-- numSteps = 1,2,3,4,5,6

thisStep : Int -> String
thisStep numSteps =
  if numSteps < 6 then "keep going" else "turn left" 

main = asText [ (thisStep 3)
              , (thisStep 4)
              ]
