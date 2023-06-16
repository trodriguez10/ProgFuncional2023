Compiler : Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler

tests : FORCE
	mkdir -p localtests
	rm -rf ./localtests/*
	cp -r tests/*.fun localtests
	for i in 1 2 3 4 5 6 7 8 9; \
         do runhaskell Compiler.hs localtests/ejemplo$$i; \
            runhaskell Compiler.hs -o localtests/ejemplo$$i; done
	for i in 1 2 3 4; \
         do runhaskell Compiler.hs localtests/ejemplo$${i}err > \
            localtests/ejemplo$${i}err.err ; done

diffs : FORCE
	mkdir -p diff_outputs
	for i in 1 2 3 4 5 6 7 8 9; \
	do \
		diff ./tests/ejemplo$${i}.c ./localtests/ejemplo$${i}.c > diff_outputs/ejemplo$${i}_diff.txt ; \
		diff ./tests/ejemplo$${i}_opt.c ./localtests/ejemplo$${i}_opt.c > diff_outputs/ejemplo$${i}_opt_diff.txt ; \
	done
	for i in 1 2 3 4; \
	do \
		diff ./tests/ejemplo$${i}err.err ./localtests/ejemplo$${i}err.err > diff_outputs/ejemplo$${i}err_diff.txt ; \
		diff ./tests/ejemplo$${i}err.fun ./localtests/ejemplo$${i}err.fun > diff_outputs/ejemplo$${i}err_fun_diff.txt ; \
	done

FORCE: ;

# RUN ORDER
# make
# make tests
# make diffs
# make clean to remove all
