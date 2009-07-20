.PHONY: check test

check test:
	r=0; \
	for f in tests/*.in; do \
	  echo "----- $$f -----"; \
	  tests/compress.awk $$f | tests/generate.awk | runhaskell || r=1; \
	done; \
	exit $$r
