# README for: testing

## V. 0.1.0.0 -- 2024-02-16

#### terminal-based user interface (TUI) for managing GHC (Glasgow Haskell Compiler) installations and Haskell tooling
* `ghcup tui`

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
