Compiler : Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler

tests : FORCE
	mkdir -p localtests
	rm -rf ./localtests/*
	cp -r tests/*.fun localtests
	for i in 1 2 3 4 5 6 7 8 9 10; \
         do runhaskell Compiler.hs localtests/ejemplo$$i; \
            runhaskell Compiler.hs -o localtests/ejemplo$$i; done
	for i in 1 2 3 4 5 6 7; \
         do runhaskell Compiler.hs localtests/ejemplo$${i}err > \
            localtests/ejemplo$${i}err.err ; done

# Run dummy test
dummy :
	@if [ -z $(file) ]; then \
		cp tests/ejemplo1.fun localtests; \
		runhaskell Compiler.hs localtests/ejemplo1; \
		runhaskell Compiler.hs -o localtests/ejemplo1; \
	else \
		cp tests/$(file).fun localtests; \
		runhaskell Compiler.hs localtests/$(file); \
		runhaskell Compiler.hs -o localtests/$(file); \
	fi

diff:
	@if [ -z $(file) ]; then \
		diff -ruN --color=always tests localtests; \
	else \
		diff -ruN --color=always tests/$(file) localtests/$(file); \
	fi

FORCE: ;
