angueraUrl : String
angueraUrl = "https://lh4.googleusercontent.com/-5Cl3PG41VqE/U514-CWceKI/AAAAAAAAHoc/8qmI_CivIno/w1034-h582-no/20140615_102442.jpg"

helloUrl : String
helloUrl = "https://lh4.googleusercontent.com/-irJgpbFvVCI/U7ri0iiTD2I/AAAAAAAAH90/NM7LyUsZLLU/w390-h694-no/20140706_155354.jpg"

-----------------------------0

hello : Element
hello = flow down [ fittedImage 300 500 helloUrl
                  , container 300 40 middle
                      (centered (toText "sheep"))
                  ] 

anguera : Element
anguera = flow down [ croppedImage (530,80) 300 500 angueraUrl
                    , container 300 40 middle
                        (centered (toText "millor company"))
                    ]

main : Element
main = flow right [ container 350 600 middle hello
                  , container 350 600 middle anguera
                  ]

