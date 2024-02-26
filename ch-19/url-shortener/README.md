# README for: url-shortener

## V. 0.1.0.0 -- 2023-12-20

#### Useful Cabal commands
* `cabal clean`
* `cabal build`
* `cabal test`
* `cabal run`

#### During Development
* `cabal repl`<br>
    _A) tests_
	```
		ghci> :l test/TestSuite
		ghci> main
	```
    _B) app_
	```
		ghci> :l app/Main
		ghci> main
	```
	_C) src_
	```
		ghci> :l src/Doodles
		ghci> life'sMeaning
	```

<br>

---

<br>

 #### Data.List
 Haskell is a lazy language. Laziness is a fundamental feature of its evaluation model. <br>
 When it comes to lists, the cons operator (:) constructs a lazy list.<br> 
 `Laziness means that the elements of a list are not computed or evaluated until they are actually needed`.

#### Data.Text.Lazy   
 It's important to note that while lists in Haskell are lazy, the `String` type is still a linked list of characters<br>
 and operations on `String` are `not as efficient as` operations on the Text type provided by the `Data.Text module`.<br>
 That's why, `when working with text data`, particularly large amounts of it, `using Text is often preferred` for performance reasons.

 `pack` function:
  * takes a list of characters and converts it into a lazy text value.
  * used when you have a list of characters (a String in Haskell) and you want to represent it as lazy text. 

 `unpack` function:
  * takes a lazy text value and converts it into a list of characters.
  * used when you have lazy text and need to operate on it using functions that expect a list of characters.



<br>

---

<br>

## Redis

##### For this exercise install Redis, if you haven't already done so
`brew search redis` <br><br>
`brew install redis`

```
Pruned 0 symbolic links and 2 directories from /usr/local
==> Caveats
==> redis
To start redis now and restart at login:
  brew services start redis
Or, if you don't want/need a background service you can just run:
  /usr/local/opt/redis/bin/redis-server /usr/local/etc/redis.conf
```

#### Confirm successful redis installation
`redis-cli -v`

#### Start redis server (press ctrl c to shutdown)
`redis-server`

#### Start redis client (press ctrl c to shutdown)

`redis-cli`

#### optionally, perform some basic operations such as `set`, `get` and `del` ...
`set my-key "Jan"` <br>
`get my-key ` <br>
`del my-key`

#### ... or display all currently stored keys
`keys *`

#### remove all entries
`flushall`

<br>

--- 

<br>

## Url Shortener App

#### let the app generate short URL and store in redis
 `http://localhost:3000/?uri=http://www.yahoo.com`

#### verify the existence of the key/value pair in redis
`get 9MK8WEV`

####  given short URL, use the app to retrieve its corresponding full-length url 
`http://localhost:3000/9MK8WEV`<br>

