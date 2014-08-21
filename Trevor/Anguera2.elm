angueraUrl : String
angueraUrl = "https://lh4.googleusercontent.com/-5Cl3PG41VqE/U514-CWceKI/AAAAAAAAHoc/8qmI_CivIno/w1034-h582-no/20140615_102442.jpg"

helloUrl : String
helloUrl = "https://lh4.googleusercontent.com/-irJgpbFvVCI/U7ri0iiTD2I/AAAAAAAAH90/NM7LyUsZLLU/w390-h694-no/20140706_155354.jpg"

----------------------------

type Photo = { url:String, caption:String }

hike : Photo
hike = { url = helloUrl, caption = "sheep" }

fiesta : Photo
fiesta = { url = angueraUrl, caption = "millor company" }

hello : Element
hello = flow down [ fittedImage 300 500 hike.url
                  , container 300 40 middle
                      (centered (toText hike.caption))
                  ]

anguera : Element
anguera = flow down [ croppedImage (530,80) 300 500 fiesta.url
                    , container 300 40 middle
                        (centered (toText fiesta.caption))
                    ]

photoBox : Element -> Element
photoBox pic =
    container 325 600 middle pic

main : Element
main = flow right [ photoBox hello
                  , photoBox anguera
                  ]
