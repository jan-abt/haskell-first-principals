# README for: monad-transformers

## V. 0.1.0.0 -- 2024-02-17

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



###    ====== Recap: Side Effects & Monads ======

In Haskell, computations with side effects are encapsulated within monadic structures to ensure purity, referential transparency, and to maintain the separation between pure and impure code. This approach aligns with the functional programming paradigm and has several benefits:
<br><br>
Purity and Referential Transparency:<br>

Haskell strives for purity, where functions produce the same output for the same input and have no side effects. This property simplifies reasoning about code, makes it easier to test, and facilitates optimizations.<
By placing side-effecting computations within monadic structures, the pure parts of the code remain unaffected. This separation allows you to reason about the pure parts independently and understand their behavior solely based on their input and output.<br><br>
Explicit Handling of Effects:<br>

Placing side effects in monads makes the presence of effects explicit in the type system. For example, the IO monad is used to represent computations with I/O side effects.
This explicit handling of effects helps in understanding and reasoning about code. When you see a function with a type involving a specific monad, you immediately know that it might involve certain side effects.<br><br>
Control over Execution Order:<br>

Monads provide a way to sequence computations, controlling the order in which side effects occur. This sequencing is achieved through monadic bind (>>=) or do notation.
Explicitly sequencing actions in monads allows you to control the flow of execution, ensuring that certain effects happen in a specified order.<br><br>
Composability:<br>

Monads are composable. You can build complex computations by combining simpler monads. This composability is particularly useful when dealing with multiple types of effects in the same program.
For example, you can combine Reader, State, and IO monads to handle configuration, mutable state, and I/O operations, respectively.<br><br>
Separation of Concerns:<br>

Monadic structures allow you to separate concerns by isolating different types of effects. For instance, the State monad is used for stateful computations, Reader for computations with read-only context, and Writer for computations producing a log or accumulating values.
This separation aids in modular design and makes it easier to reason about different aspects of a program independently.


---
<br>

#### Incorrect Example
```
   sample :: String
   sample = 
       putStrLn "hello" -- Produces a side effect of type IO ()
       "my String"      -- Returns a pure String, causing a type mismatch
```
The attempt above is incorrect because it tries to combine two separate actions, or 
expressions, within the same block. <br>
* `putStrLn "hello"` is an action that produces an `IO ()` side effect. <br>
* `"my String"` is a pure `String` expression. <br>


To perform both, the side effect and to obtain a result, <br> 
we need to use monadic constructs like `do` notation or the bind (>>=) operator. <br>
This ensures that the sequencing of actions is done within the context of the appropriate monad.<br>

#### Correct Example
* _using `(>>=)`, the `bind` operator_

	The bind operator allows us to sequence IO actions and use their results.<br>
	``` 
	sampleBind :: IO String
	sampleBind = 
			putStrLn "hello" >>= \_ -> return "my String"
	```
	Note that we could have ignored the result of the first action `()` explicitly by using `(>>)`<br>
	``` 
			putStrLn "print me" >> return "my String"    
	``` 
	This is because in our example we are not expecting to thread-in the result of<br>
	the first action, here void `()`, into the second.

* _using `do`, the do-notaion_

	Placing our operations inside a do-block provides a very readable way to sequence IO actions.<br>
	``` 
	sampleDo :: IO String
	sampleDo = do
			_ <- putStrLn "hello"
			return "my String"
	```  