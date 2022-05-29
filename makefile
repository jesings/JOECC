CC = gcc
LDFLAGS = -lpthread
CFLAGS = -ggdb -g3 -gdwarf -Wall -Wextra -Wwrite-strings -Winit-self -Wcast-align -Wcast-qual -Wpointer-arith -Wstrict-aliasing -Wformat=2 -Wmissing-declarations -Wmissing-include-dirs -Wno-unused-parameter -Wuninitialized -Wold-style-definition -Wstrict-prototypes -Wmissing-prototypes -march=native -D USEGCC
LEXFLAGS = -d -Cfe --yylineno
compilerasan: CFLAGS += -fsanitize=address
compilerasan: LDFLAGS += -fsanitize=address
compilerasan: compiler
nodebug: CFLAGS = -O2 -D NODEBUG -march=native
nodebug: compiler
useclang: CFLAGS += -D USECLANG
useclang: CC = clang
useclang: compiler
usemusl: CFLAGS += -D USEMUSL -fsanitize=address
usemusl: LDFLAGS += -fsanitize=address
usemusl: compiler
perftest: CFLAGS = -O2 -D NODEBUG -g -march=native
perftest: LEXFLAGS = -Cfer -p -p
perftest: profile
CFLAGS += -D HEADERS_VERSION=\"$(shell $(CC) --version | grep "[0-9]\+\.[0-9]\+\.[0-9]\+" -o)\"

SRCS = lex.yy.c joecc.tab.c ifjoecc.tab.c 3ac.c codegen.c compintern.c compmain.c dynarr.c dynstr.c opt.c printree.c qhash.c reg.c ssa.c treeduce.c
OBJS = $(SRCS:.c=.o)

compiler: lex.yy.c joecc.tab.c ifjoecc.tab.c $(OBJS)
	$(CC) $(OBJS) -o compiler $(LDFLAGS)
lex.yy.c: joecc.lex
	flex $(LEXFLAGS) joecc.lex
%.tab.c: %.y
	bison -d $<
%.o: %.c
	$(CC) $< -c $(CFLAGS)


profile: compiler
	perf record -g --call-graph=dwarf ./compiler *.c *.h
perfreport:
	perf report -g fractal -F+period,srcline
flamegraph:
	perf script | ~/build/FlameGraph/stackcollapse-perf.pl > out.perf-folded #replace with the right directory
	~/build/FlameGraph/flamegraph.pl out.perf-folded > perf.svg #replace with the right directory

clean:
	-rm *.o *.joecco *.joeccs ./functions/* *.tab.* ./compiler precompilation*
