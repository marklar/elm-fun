image1URL = "https://lh4.googleusercontent.com/-ljPWuYrjb0Q/U7kmm_6DpAI/AAAAAAAAH4c/bO5h1NuhHQc/w390-h694-no/20140706_111743.jpg"
image2URL = "https://lh6.googleusercontent.com/-T1LrdrmtKmI/U8AGP_gOEdI/AAAAAAAAIC8/3AKx9tUqdAE/w390-h694-no/20140711_174133.jpg"
---------------

main : Element
main = flow right [ fittedImage 250 300 image2URL
                  , fittedImage 250 300 image1URL
                  ]
