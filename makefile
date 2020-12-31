CC = gcc
LDFLAGS = -lpthread
CFLAGS = -ggdb -g3 -Wall -Wextra -Wwrite-strings -Winit-self -Wcast-align -Wcast-qual -Wpointer-arith -Wstrict-aliasing -Wformat=2 -Wmissing-declarations -Wmissing-include-dirs -Wno-unused-parameter -Wuninitialized -Wold-style-definition -Wstrict-prototypes -Wmissing-prototypes -march=native
LEXFLAGS = -d -Cfe --yylineno
compilerasan: CFLAGS += -fsanitize=address
compilerasan: LDFLAGS += -fsanitize=address
compilerasan: compiler
nodebug: CFLAGS = -O2 -D NODEBUG -ggdb -g3 -march=native
nodebug: LEXFLAGS = -d -Cfer -p -p
nodebug: LDFLAGS +=
nodebug: compiler
compiler: joecc.tab.o lex.yy.o ifjoecc.tab.o hash.o fixedhash.o  dynarr.o compintern.o compmain.o dynstr.o printree.o parallel.o treeduce.o 3ac.o opt.o ssa.o dstore.o
	$(CC) joecc.tab.o lex.yy.o ifjoecc.tab.o hash.o fixedhash.o dynarr.o compintern.o compmain.o dynstr.o printree.o parallel.o treeduce.o 3ac.o opt.o ssa.o dstore.o -o compiler $(LDFLAGS)
gotest: compiler
	./compiler dynarr.c
lex.yy.c: joecc.lex
	flex $(LEXFLAGS) joecc.lex
joecc.tab.c: joecc.y
	bison -d joecc.y
ifjoecc.tab.c: ifjoecc.y
	bison -d ifjoecc.y
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
ifjoecc.tab.o: ifjoecc.tab.c
	$(CC) ifjoecc.tab.c -c $(CFLAGS)
compintern.o: compintern.c
	$(CC) compintern.c -c $(CFLAGS)
compmain.o: compmain.c
	$(CC) compmain.c -c $(CFLAGS)
printree.o: printree.c
	$(CC) printree.c -c $(CFLAGS)
parallel.o: parallel.c
	$(CC) parallel.c -c $(CFLAGS)
3ac.o: 3ac.c
	$(CC) 3ac.c -c $(CFLAGS)
opt.o: opt.c
	$(CC) opt.c -c $(CFLAGS)
ssa.o: ssa.c
	$(CC) ssa.c -c $(CFLAGS)
dstore.o: dstore.c
	$(CC) dstore.c -c $(CFLAGS)

clean:
	-rm *.o
	-rm ./dstest
	-rm *.tab.*

dstest.o: dstest.c
	$(CC) dstest.c -c $(CFLAGS)
dstest: dynarr.o hash.o dstest.o
	$(CC) dynarr.o hash.o dstest.o -o dstest
	./dstest
