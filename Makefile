.PHONY: check test

check test:
	r=0; \
	for f in tests/*.in; do tests/generate $$f | runhaskell || r=1; done; \
	exit $$r
