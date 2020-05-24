CC = gcc
CFLAGS = -g
lex.yy.o: lex.yy.c highCs.tab.c
	gcc -c lex.yy.c
	gcc -c highCs.tab.c

lex.yy.c: highCs.lex 
	flex highCs.lex
highCs.tab.c: highCs.y
	bison highCs.y --defines
hash.o: hash.c
	$(CC) hash.c -c $(CFLAGS)
dynarr.o: dynarr.c
	$(CC) dynarr.c -c $(CFLAGS)
lex.yy.o: lex.yy.c
	$(CC) lex.yy.c -c $(CFLAGS)
highCs.tab.o: highCs.tab.c
	$(CC) highCs.tab.c -c $(CFLAGS)

clean:
	-rm *.o
	-rm ./dstest
	-rm *.tab.*

dstest.o: dstest.c
	$(CC) dstest.c -c $(CFLAGS)
dstest: dynarr.o hash.o dstest.o
	$(CC) dynarr.o hash.o dstest.o -o dstest
	./dstest
