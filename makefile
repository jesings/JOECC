CC = gcc
LDFLAGS = 
CFLAGS = -g
compiler: highCs.tab.o lex.yy.o hash.o dynarr.o conv.o compintern.o
	$(CC) highCs.tab.o lex.yy.o hash.o dynarr.o conv.o compintern.o -o compiler $(LDFLAGS)
lex.yy.c: highCs.lex
	flex --header-file=lex.h highCs.lex
highCs.tab.c: highCs.y
	bison -d highCs.y
hash.o: hash.c
	$(CC) hash.c -c $(CFLAGS)
dynarr.o: dynarr.c
	$(CC) dynarr.c -c $(CFLAGS)
conv.o: conv.c
	$(CC) conv.c -c $(CFLAGS)
lex.yy.o: lex.yy.c
	$(CC) lex.yy.c -c $(CFLAGS)
highCs.tab.o: highCs.tab.c
	$(CC) highCs.tab.c -c $(CFLAGS)
compintern.o: compintern.c
	$(CC) compintern.c -c $(CFLAGS)

clean:
	-rm *.o
	-rm ./dstest
	-rm *.tab.*

dstest.o: dstest.c
	$(CC) dstest.c -c $(CFLAGS)
dstest: dynarr.o hash.o dstest.o
	$(CC) dynarr.o hash.o dstest.o -o dstest
	./dstest
