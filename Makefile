all: Build Run
Build:
	stack build --allow-different-user --ghc-options="-Wall -fwarn-incomplete-uni-patterns -j4 +RTS -A128m -n2m -RTS -threaded"
Run:
	stack exec DataStructures-exe --allow-different-user
Test:
	stack test
GhcidTest:
	ghcid -stack -W -T Test.main
