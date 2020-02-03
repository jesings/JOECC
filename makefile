CFLAGS = -g
lex.yy.c: highCs.lex highCs.tab.c
	flex highCs.lex
highCs.tab.c: highCs.y
	bison highCs.y --defines
hash.o: hash.c
	gcc hash.c -c $(CFLAGS)
dynarr.o: dynarr.c
	gcc dynarr.c -c $(CFLAGS)
lex.yy.o: lex.yy.c
	gcc lex.yy.c -c $(CFLAGS)
highCs.tab.o: highCs.tab.c
	gcc highCs.tab.c -c $(CFLAGS)

