# JOECC
Jonathan's Optimizing and Extensible C Compiler (pronounced "joke"), called as such because the name, just like the compiler itself, is a poorly written joke.

JOECC is intended to be ANSI-opinionated, mostly conforming to C99, C11, etc. however it is not intended to be completely ANSI compliant or even to necessarily implement any defined subset of ANSI functionality. It is intended to compile most pre-existing well defined C99 compliant code.

NOTE: JOECC is a work-in progress, and currently cannot fully compile code

- [x] Lexer
- [x] Parser
- [ ] Preprocessor (part of lexer) (bugfixing [with macro preparsing])
- [x] AST generation
- [x] AST optimization
- [x] AST display using graphviz
- [ ] AST type annotation (in progress)
- [ ] 3 address code generation (in progress)
- [ ] Static Single Assignment conversion
- [ ] General Optimizations (to be enumerated later)
- [ ] Register Renaming
- [ ] Conversion back from Single Static Assignment
- [ ] Assembly generation
- [ ] Machine code generation
- [ ] Machine code optimization
- [ ] Linker and loader backend