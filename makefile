CC = gcc
LDFLAGS = -pthread
CFLAGS = -g 
compiler: joecc.tab.o lex.yy.o hash.o dynarr.o conv.o compintern.o compmain.o preparse.o
	$(CC) joecc.tab.o lex.yy.o hash.o dynarr.o conv.o compintern.o compmain.o preparse.o -o compiler $(LDFLAGS)
gotest: compiler
	./compiler < dynarr.c
lex.yy.c: joecc.lex
	flex --header-file=lex.h joecc.lex
joecc.tab.c: joecc.y
	bison -d joecc.y
hash.o: hash.c
	$(CC) hash.c -c $(CFLAGS)
dynarr.o: dynarr.c
	$(CC) dynarr.c -c $(CFLAGS)
conv.o: conv.c
	$(CC) conv.c -c $(CFLAGS)
lex.yy.o: lex.yy.c
	$(CC) lex.yy.c -c $(CFLAGS)
joecc.tab.o: joecc.tab.c
	$(CC) joecc.tab.c -c $(CFLAGS)
compintern.o: compintern.c
	$(CC) compintern.c -c $(CFLAGS)
compmain.o: compmain.c
	$(CC) compmain.c -c $(CFLAGS)
preparse.o: preparse.c
	$(CC) preparse.c -c $(CFLAGS)

clean:
	-rm *.o
	-rm ./dstest
	-rm *.tab.*

dstest.o: dstest.c
	$(CC) dstest.c -c $(CFLAGS)
dstest: dynarr.o hash.o dstest.o
	$(CC) dynarr.o hash.o dstest.o -o dstest
	./dstest
