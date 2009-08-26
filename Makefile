.PHONY: check test prof mostlyclean clean

EXE = under
SRC = Codec/Binary/DER.hs Main.hs
PROF_OPTS = -prof -auto-all -caf-all

check test:
	runhaskell -- -fwarn-unused-imports -DTESTING Tests/Main.hs

prof:
	ghc $(PROF_OPTS) -fforce-recomp --make -o $(EXE) Main.hs

mostlyclean:
	rm -f $(SRC:.hs=.hi) $(SRC:.hs=.o)\
 $(foreach x,prof hp aux ps,$(EXE).$(x))

clean: mostlyclean
	rm -f $(EXE)
