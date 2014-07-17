urlBase = "https://lh4.googleusercontent.com/"

imageUrls =
  [ "-glL_vSgmS7A/U8AGaNLjqjI/AAAAAAAAID4/4duLz8q7kok/w390-h694-no/20140711_174414.jpg"
  , "-HgkU_vze4g4/U76FBYJXNtI/AAAAAAAAIBM/A4drjOTO-HQ/w390-h694-no/20140710_130812.jpg"
  , "-sLZntQ5KnnI/U7knWDiVDRI/AAAAAAAAH7Q/WmkZL3As3i4/w390-h694-no/20140705_174617.jpg"
  ]

imageAndCaption =
  zip imageUrls [ "Zo-bot a-bouncin'"
                , "The Zozers feelin' tired"
                , "Maiele diggin' the waterfall"
                ]

mkImageWithCaption url caption = 
  container 250 350 middle (
      flow down [ fittedImage 200 300 (urlBase ++ url)
                , plainText caption
                ])

main =
  flow right
    (map (\(u,c) -> mkImageWithCaption u c) imageAndCaption)
