oct = group
  [ filled red (ngon 8 50)
  , outlined { defaultLine | width <- 5
                           , color <- red
                           , cap <- Round } (ngon 8 60)
  ]

main =
    collage 300 300
      [ rotate (degrees 23) oct
      , toForm (centered (style { defaultStyle | color <- white } (toText "STOP")))
      ]
