VERSION = $(shell gcc --version | grep "[0-9]\+\.[0-9]\+\.[0-9]\+" -o)
CC = gcc
LDFLAGS = -lpthread
CFLAGS = -ggdb -g3 -gdwarf -Wall -Wextra -Wwrite-strings -Winit-self -Wcast-align -Wcast-qual -Wpointer-arith -Wstrict-aliasing -Wformat=2 -Wmissing-declarations -Wmissing-include-dirs -Wno-unused-parameter -Wuninitialized -Wold-style-definition -Wstrict-prototypes -Wmissing-prototypes -march=native -D USEGCC
LEXFLAGS = -d -Cfe --yylineno
compilerasan: CFLAGS += -fsanitize=address
compilerasan: LDFLAGS += -fsanitize=address
compilerasan: compiler
nodebug: CFLAGS = -O2 -D NODEBUG -ggdb -g3 -march=native -D HEADERS_VERSION=\"$(VERSION)\"
nodebug: LDFLAGS +=
nodebug: compiler
useclang: VERSION = $(shell clang --version | grep "[0-9]\+\.[0-9]\+\.[0-9]\+" -o)
useclang: CFLAGS += -D USECLANG
useclang: compiler
usemusl: CFLAGS += -D USEMUSL
usemusl: CFLAGS += -fsanitize=address
usemusl: LDFLAGS += -fsanitize=address
usemusl: compiler
perftest: CFLAGS = -O2 -D NODEBUG -g -march=native -D HEADERS_VERSION=\"$(VERSION)\"
perftest: LEXFLAGS = -Cfer -p -p
perftest: profile
CFLAGS += -D HEADERS_VERSION=\"$(VERSION)\"
compiler: joecc.tab.o lex.yy.o ifjoecc.tab.o dynarr.o compintern.o compmain.o dynstr.o printree.o treeduce.o 3ac.o opt.o ssa.o reg.o codegen.o qhash.o
	$(CC) joecc.tab.o lex.yy.o ifjoecc.tab.o dynarr.o compintern.o compmain.o dynstr.o printree.o treeduce.o 3ac.o opt.o ssa.o reg.o codegen.o qhash.o -o compiler $(LDFLAGS)
gotest: compiler
	./compiler dynarr.c
lex.yy.c: joecc.lex
	flex $(LEXFLAGS) joecc.lex
joecc.tab.c: joecc.y
	bison -d joecc.y
ifjoecc.tab.c: ifjoecc.y
	bison -d ifjoecc.y
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
3ac.o: 3ac.c
	$(CC) 3ac.c -c $(CFLAGS)
opt.o: opt.c
	$(CC) opt.c -c $(CFLAGS)
ssa.o: ssa.c
	$(CC) ssa.c -c $(CFLAGS)
codegen.o: codegen.c
	$(CC) codegen.c -c $(CFLAGS)
reg.o: reg.c
	$(CC) reg.c -c $(CFLAGS)
qhash.o: qhash.c
	$(CC) qhash.c -c $(CFLAGS)

profile: compiler
	perf record -g --call-graph=dwarf ./compiler *.c *.h

perfreport:
	perf report -g fractal -F+period,srcline

flamegraph:
	perf script | ~/build/FlameGraph/stackcollapse-perf.pl > out.perf-folded #replace with the right directory
	~/build/FlameGraph/flamegraph.pl out.perf-folded > perf.svg #replace with the right directory




clean:
	-rm *.o *.joecco *.joeccs
	-rm ./functions/*
	-rm *.tab.*
	-rm ./compiler
	-rm precompilation*
