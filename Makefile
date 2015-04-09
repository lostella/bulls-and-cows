all:	bullscows.hs
	ghc -O3 bullscows.hs

clean:	
	rm bullscows bullscows.o bullscows.hi
