# purescript-erl-simplebus

Very simple message bus built on top of Gproc (and as such that dependency needs adding to rebar.config)

# Defining a bus and message type

```purescript

    data BookEvent = BookCreated Isbn
               | BookUpdated Isbn
               | BookDeleted Isb

    bus :: SimpleBus.Bus String BookEvent
    bus = SimpleBus.bus "book_library"

```    
    

# Sending Messages on the bus

```purescript

   SimpleBus.raise bus (BookCreated book.isbn)
   
```
# Subscribing to Messages


```purescript

  _ <- SimpleBus.subscribe BookLibrary.bus handleMessage
  
  handleMessage :: BookEvent :: Effect Unit
  handleMessage ev = pure unit
  
```
