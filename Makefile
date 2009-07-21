.PHONY: check test

check test:
	runhaskell -- -fwarn-unused-imports Tests/Main.hs
