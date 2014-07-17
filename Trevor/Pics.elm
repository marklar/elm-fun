imagesWithCaption =
  [ ("https://lh4.googleusercontent.com/-irJgpbFvVCI/U7ri0iiTD2I/AAAAAAAAH90/NM7LyUsZLLU/w390-h694-no/20140706_155354.jpg",
     "Trevzors greeting some sheep")
  , ("https://lh3.googleusercontent.com/-ic9VnNnUJQE/U5TB1tjckMI/AAAAAAAAHe8/AXtftPiOus0/w1034-h582-no/20140608_194643.jpg",
     "Kainoa con un gato zaragozano")
  , ("https://lh5.googleusercontent.com/-fPorvM7t3yQ/U3c9Qiyx5RI/AAAAAAAAHD0/OI5_gTFLEvU/w390-h694-no/20140517_124259.jpg",
     "The Trev-bot losin' to his Baba")
  ]
  
main =
  flow right
    [ container 250 350 middle
        (flow down
           [ fittedImage 200 300 "https://lh4.googleusercontent.com/-irJgpbFvVCI/U7ri0iiTD2I/AAAAAAAAH90/NM7LyUsZLLU/w390-h694-no/20140706_155354.jpg"
           , plainText "Trevzors greeting some sheep"
           ])
    ]
