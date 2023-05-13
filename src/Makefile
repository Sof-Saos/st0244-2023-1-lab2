GHCF := -Werror -Wall -Wmissing-local-signatures

.PHONY: clean

all : compile run

compile: Tests.hs 
	ghc $(GHCF) -o lab2 Tests.hs

run: lab2.exe
	.\lab2.exe

clean: 
	del *.o *.hi lab2.exe
#For linux rm -f *.o *.hi lab2.exe 

versions:
	@ghc --version 
	@cabal --version
	@powershell -command "gc .ghc.environment.x86_64-mingw32-9.6.1"
#For linux, use the next command >cat .ghc.environment.x86_64-mingw32-9.6.1
