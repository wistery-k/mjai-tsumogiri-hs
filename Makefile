GHC = ghc --make
GHCFLAGS = -O2 -Wall

.PHONY: main clean

main:
	$(GHC) $(GHCFLAGS) Main.hs

clean:
	find . -name "*.o" -o -name "*.hi" | xargs $(RM) -R
	$(RM) Main