type Pic = { name: String
           , url: String
           , caption: String
           }

pics : [Pic]
pics = [ { name = "Tramp"
         , url = "https://lh6.googleusercontent.com/-T1LrdrmtKmI/U8AGP_gOEdI/AAAAAAAAIC8/3AKx9tUqdAE/w390-h694-no/20140711_174133.jpg"
         , caption = "Me jumping on the tramp"
         }
       , { name = "Falls"
         , url = "https://lh4.googleusercontent.com/-ljPWuYrjb0Q/U7kmm_6DpAI/AAAAAAAAH4c/bO5h1NuhHQc/w390-h694-no/20140706_111743.jpg"
         , caption = "Me & the Seven Sisters Falls"
         }
       ]

---------------

picWidth  = 250
picHeight = 300

picNCapt : Pic -> Element
picNCapt {url,caption} =
    container (picWidth + 50) (picHeight + 100) middle
      (flow up [ container picWidth 50 middle
                   (centered (toText caption))
               , fittedImage picWidth picHeight url
               ])

main : Element
main = flow right (map picNCapt pics)
