# JOECC
Jonathan's Optimizing and Extensible C Compiler (pronounced "joke"), called as such because the name, just like any attempt to use the compiler in place of an industrial strength one like gcc or clang, is a poorly written joke.

JOECC is intended to be ANSI-opinionated, mostly conforming to C99, C11, etc. however it is not intended to be completely ANSI compliant or even to necessarily implement any defined subset of ANSI functionality. It is intended to compile most pre-existing well defined C99 compliant code.

NOTE: JOECC is a work-in progress, and currently cannot fully compile code

- [x] Lexer
- [x] Parser
- [x] Preprocessor (part of lexer)
- [x] AST generation
- [x] AST optimization
- [x] AST type annotation
- [x] 3 address code generation
- [x] Static Single Assignment conversion
- [x] Constant Folding
- [x] Global Value Numbering
- [ ] Better Arithmetic Reassociation
- [ ] GVN Based Partial redundancy elimination (in progress)
- [ ] Sparse Conditional Constant Propagation
- [ ] Pointer Analysis
- [ ] Scalar Replacement of Aggregates
- [ ] Other Optimizations (to be enumerated further later)
- [ ] Register Allocation (in progress)
- [ ] Assembly generation
- [ ] Machine code generation
- [ ] Machine code optimization
- [ ] Linker backend

# JOECCIR

JOECCIR (pronounced joker) is JOECC's 3 address code intermediate representation.

### Control flow

JOECCIR comes in the form of a control flow graph (CFG). This CFG is made up of many basic blocks which are regions in which no branching occurs. Control flow in the function starts at a single block, and ends at a different single block, provided the program doesn't infinitely loop. Each basic block may contain a linked list of JOECCIR operations, or no operations.

### Visualization

When running JOECC in debug mode, it will create in the working directory a folder called functions, and put in it the JOECCIR CFG output by each compilation pass. Currently the passes which give debug information are \_justssa and \_withgvn. For each function in the code compiled, provided that it compiled successfully, there will be dot files which can render the CFG after each pass. For example, if I compiled a file containing the function "main" there will be 2 files: functions/main_justssa.dot and functions/main_withgvn.dot. In order to view the graph for such a file install the graphviz package, and simply run `dot -Tx11 functions/main_justssa.dot`. It can also be converted to an svg with `-Tsvg` or into a gif (pronounced gif) with `-Tgif`.

In this representation, it should be pretty evident as to what basic blocks are, if they have operations. If not, they will just appear as a long hex number with no box around it. Different kinds of operands are colored, as are the edges between basic blocks. The blocks that are visited next if the branching condition is not met (which I call next blocks) have a blue edge to them, and the blocks that are visited if the branching condition is met (which I call branchblocks) have a red edge to them. JOECCIR is specifically designed so that basic blocks may have a maximum of 2 out edges.

### Operand formats

Operands come in a number of formats:

- Registers: registers can come in a number of formats--if the register corresponds to a program variable, it is written as \<variable\_name\>\_\<registernumber\>, and if not it is written as reg\<registernumber\>. Registers are suffixed with the type and length of the operand (i.e. s32 for a signed 32 bit integer, u8 for an unsigned 8 bit integer, or f64 for a 64 bit double precision floating point number). Registers which correspond to variables whose address is taken in the program may not be given a register number suffix

- Labels: labels refer to either a label for a program, or a global variable. They are written in curly braces like {\<labelname\>}. They are similarly suffixed with the type of the type and length being referred to if they are being used as data.

- Dereferences: dereferences are a modification to register or label operands. They consist of either a label or a 64 bit unsigned integer register operand, with a flag set that the operand should be dereferenced within the operation. They are written the same as the register or label surrounded by parentheses. The type of the dereference operand refers to the type pointed to, rather than the type of the pointer, and is written after the closing parenthesis.

- Constants: integer and float literals can also be operands. They are all assumed to be 64 bits, and written just as their corresponding literal. String constants are allowed too, and they are treated just as the other constants, and assumed to be 64 bit unsigned integers.

### Operation Table

JOECCIR consists of the following operations

Operation | Description of functionality|Operands needed
----------|-----------------------------|---------------
NOP_3|No-op operation|None
ADD_U, ADD_F|Adds source operands as integers and floating point numbers respectively|2 source and 1 dest
SUB_U, SUB_F|Subtracts source operand 2 from source operand 1 as integers and floating point numbers respectively|2 source and 1 dest
MULT_U, MULT_I, MULT_F|Multiplies source operands as unsigned integers, signed integers, and floating point numbers respectively|2 source and 1 dest
DIV_U, DIV_I, DIV_F|Divides source operand 1 by source operand 2 as unsigned integers, signed integers, and floating point numbers respectively|2 source and 1 dest
MOD_U, MOD_I|Yields to dest remainder of source operand 1 divided by source operand 2 as unsigned integers, signed integers, and floating point numbers respectively|2 source and 1 dest
NEG_I, NEG_F|Negates source signed integer and floating point number operand respectively|1 source and 1 dest
F2I, I2F|Converts floating point number source operand to dest integer operand or vice-versa|1 source and 1 dest
AND_U, OR_U, XOR_U|bitwise ANDs, ORs, and XORs respectively source integer operands|2 source and 1 dest
NOT_U|bitwise NOTs source integer operand|1 source and 1 dest
SHL_U, SHL_I|Yields to dest source operand 1 shifted to the left by the amount in source operand 2 using bitwise and arithmetic shifting respectively|2 source and 1 dest
SHR_U, SHR_I|Yields to dest source operand 1 shifted to the right by the amount in source operand 2 using bitwise and arithmetic shifting respectively|2 source and 1 dest
EQ_U, EQ_F|Assigns 1 to integer dest operand if the source operands are equal integers and floating point numbers respectively, otherwise assigns 0|2 source and 1 dest
NE_U, NE_F|Assigns 0 to integer dest operand if the source operands are equal integers and floating point numbers respectively, otherwise assigns 1|2 source and 1 dest
GE_U, GE_I, GE_F|Assigns 1 to integer dest operand if source operand 1 is greater than or equal to source operand 2 for unsigned integers, signed integers, and floating point numbers respectively, otherwise assigns 0|2 source and 1 dest
LE_U, LE_I, LE_F|Assigns 1 to integer dest operand if source operand 1 is less than or equal to source operand 2 for unsigned integers, signed integers, and floating point numbers respectively, otherwise assigns 0|2 source and 1 dest
GT_U, GT_I, GT_F|Assigns 1 to integer dest operand if source operand 1 is greater than source operand 2 for unsigned integers, signed integers, and floating point numbers respectively, otherwise assigns 0|2 source and 1 dest
LT_U, LT_I, LT_F|Assigns 1 to integer dest operand if source operand 1 is less than source operand 2 for unsigned integers, signed integers, and floating point numbers respectively, otherwise assigns 0|2 source and 1 dest
BEQ_U, BEQ_F|Directs codegen to go to the branching block if the source operands are equivalent signed integer, unsigned integer, and floating point numbers respectively|2 source and 0 dest
BGE_U, BGE_I, BGE_F|Directs codegen to go to the branching block if source operand 1 is greater than or equal to source operand 2 as signed integers, unsigned integers, and floating point numbers respectively|2 source and 0 dest
BGT_U, BGT_I, BGT_F|Directs codegen to go to the branching block if source operand 1 is greater than source operand 2 as signed integers, unsigned integers, and floating point numbers respectively|2 source and 0 dest
BNZ_3, BEZ_3|Directs codegen to go to the branching block if source integer operand equals zero or does not equal zero respectively|1 source and 0 dest
JEQ_I|Directs codegen to go to the branching block if the source operands are equivalent signed integers, the dest operand specifies the label to branch to; only generated in compiling switch cases|2 source and 1 dest
ADDR_3|Gets the address of label or dereferenced source operand 1 and puts it in dest|1 source and 1 dest
COPY_3|Copies from source integer (pointer) operand 1 into the dest integer (pointer) operand the number of bytes specified in source integer (length) oprand 2|2 source and 1 dest
ARROFF|Moves from the integer source operand 2th element in the array pointed to by integer (pointer) source operand 1 to the dest operand, the size of each array elemnt is implicit in the size of source operand 1 |2 source and 1 dest
ARRMOV|Moves from integer source operand 1 to the integer source operand 2th element in the array pointed to by integer (pointer) operand dest, the size of each array element is implicit in the size of the dest operand|2 source and 1 dest
MTP_OFF|Moves from memory pointed to by integer (pointer) source operand 1 offset by the integer source operand 2th element to the dest operand|2 source and 1 dest
ALOC_3|Allocates integer source operand 1 bytes of stack memory, places address of allocated memory into dest operand|1 source and 1 dest
DEALOC_3|Deallocates integer source operand 1 bytes of stack memory|1 source and 0 dest
LBL_3|Label pseudo-operation|1 source operand for label's name
ARG_3|Specifies that source operand 1 will be passed to the function called immediately after the last arg|1 source
CALL_3|Calls the function specified by the label or integer (pointer) in the source operand, and outputs the result to the dest operand|1 source and 1 dest
RET_3|Return from function, yielding value in source operand or nothing if source operand is garbage|1 source
INIT_3|Initializes dest operand without a value, used in case of use before assignment|1 dest
PARAM_3|Specifies that the next parameter will be passed into the specified dest operand|1 dest
PHI|SSA specific; Moves into the dest operand the source operand corresponding to the index of the inedge that the control flow took to the current block; PHI statements always precede all others in a block|n source 1 dest
ASM|Has information from asm statement, passed straight through to codegen|N/A (WIP)
