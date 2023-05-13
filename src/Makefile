GHCF := -Werror -Wall -Wmissing-local-signatures

.PHONY: clean

all : compile run

compile: Tests.hs 
	ghc $(GHCF) -o lab2 Tests.hs

run: lab2.exe
	.\lab2.exe

clean: 
	del *.o *.hi lab2.exe
#rm -f *.o *.hi lab2.exe use if you're working on linux


