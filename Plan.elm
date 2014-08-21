import Window

-- main = container 1000 1000 (topLeftAt (absolute 100) (absolute 100)) [markdown|

main : Signal Element
main = page <~ html

page : Element -> Element
page el = flow left [ spacer 200 2000, el ]

html : Signal Element
html = htmlBox <~ Window.width

htmlBox : Int -> Element
htmlBox w = width w [markdown|

# Data, Functions

Functions: what we do.

Data: what we do them to.


## "data"

also: "information"


### Tuples

`Mouse.position : Signal (Int,Int)`

The `(Int,Int)` part is a 'tuple'.  A tuple groups those two related
pieces of info.  In this case they both happen to be `Int`, but they
could be anything.

Tuples are for when you have a small number of related things (pieces
of data) which together you think of as just *one* thing.

In a tuple, the elements inside aren't given names.  Luckily, we
happen to know the meaning: the first is X coord, second is Y coord.


### Records

`Keyboard.arrows : Signal { x:Int, y:Int }`

This is a 'record'.  A couple of pieces of related info which are
*named*.  Especially good for when you have more than just a couple of
things.


### Lists

For storing some number -- could be 0, could be 1000 -- things all of
the same type.

Not like a tuple or record: those are for creating one thing.

When we have lists of things, we can 


### Type Aliases

Let's say you have record (which is for storing a book):

    book1 : { title:String, author:String, pages:Int }
    book1 = { title="Diary", author="Me", pages=176 }

You see that `book1` already has a type, which itself looks almost
just like the record.

You can make a type alias, to make it easier to understand that (and
so that you have less to write each time you want to use it):

    type Book = { title:String, author:String, pages:Int }

So now we can do this, instead:

    book1 : Book
    book1 = { title="Diary", author="Me", pages=176 }

And if we had more than one book, we could store them in a List:

    book2 : Book
    book2 = { title="Diary", author="Someone Else", pages=176 }

    someBooks : [Book]
    someBooks = [ book1, book2 ]
|]
