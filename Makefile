all: Build Run
Build:
	stack build --allow-different-user --ghc-options="-Wall -fwarn-incomplete-uni-patterns"
Run:
	stack exec DataStructures-exe --allow-different-user
Test:
	stack test
