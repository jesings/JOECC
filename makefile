CC = gcc
LDFLAGS = 
CFLAGS = -g 
compiler: joecc.tab.o lex.yy.o hash.o fixedhash.o  dynarr.o compintern.o compmain.o dynstr.o printree.o parallel.o treeduce.o
	mkdir -p functions
	$(CC) joecc.tab.o lex.yy.o hash.o fixedhash.o dynarr.o compintern.o compmain.o dynstr.o printree.o parallel.o treeduce.o -o compiler $(LDFLAGS)
gotest: compiler
	./compiler dynarr.c
lex.yy.c: joecc.lex
	flex --header-file=lex.h joecc.lex
joecc.tab.c: joecc.y
	bison -d joecc.y --report=all
hash.o: hash.c
	$(CC) hash.c -c $(CFLAGS)
fixedhash.o: fixedhash.c
	$(CC) fixedhash.c -c $(CFLAGS)
dynarr.o: dynarr.c
	$(CC) dynarr.c -c $(CFLAGS)
dynstr.o: dynstr.c
	$(CC) dynstr.c -c $(CFLAGS)
lex.yy.o: lex.yy.c
	$(CC) lex.yy.c -c $(CFLAGS)
joecc.tab.o: joecc.tab.c
	$(CC) joecc.tab.c -c $(CFLAGS)
compintern.o: compintern.c
	$(CC) compintern.c -c $(CFLAGS)
compmain.o: compmain.c
	$(CC) compmain.c -c $(CFLAGS)
printree.o: printree.c
	$(CC) printree.c -c $(CFLAGS)
parallel.o: parallel.c
	$(CC) parallel.c -c $(CFLAGS)

clean:
	-rm *.o
	-rm ./dstest
	-rm *.tab.*

dstest.o: dstest.c
	$(CC) dstest.c -c $(CFLAGS)
dstest: dynarr.o hash.o dstest.o
	$(CC) dynarr.o hash.o dstest.o -o dstest
	./dstest
