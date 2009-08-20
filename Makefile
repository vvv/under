.PHONY: check test

check test:
	runhaskell -- -fwarn-unused-imports -DTESTING Tests/Main.hs
