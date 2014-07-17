
redSquare = outlined (dashed red) (square 90)
diamond = rotate (degrees 45) redSquare

blackOval = filled black (oval 80 89)

sign = group [diamond, blackOval]

main = collage 500 600 [ scale 2 sign ]
