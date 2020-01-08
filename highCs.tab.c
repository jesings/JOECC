/* A Bison parser, made by GNU Bison 3.5.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1





# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_HIGHCS_TAB_H_INCLUDED
# define YY_YY_HIGHCS_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 21 "highCs.y"

#include <string.h>

typedef enum{
  undefined,
  function,
  parameter,
  local_var,
  global_var
} IDTYPE;
typedef struct {
  IDTYPE type;
  //int index;//index of type within scope (i.e. parameter index)
  char* name;
} IDENTIFIER;

typedef enum{
  INT8, INT16, INT32, INT64, BYTE, DBYTE, QBYTE, OBYTE, SINGLE, DOUBLE
} VARTYPE;

typedef enum{
  NOP, STRING, INT, FLOAT, IDENT,
  ADD, NEG, SUB, EQ, NEQ, GT, LT, GTE, LTE, MULT, DIVI, MOD,
  PREINC, POSTINC, PREDEC, POSTDEC,
  L_AND, L_OR, L_NOT, B_AND, B_OR, B_XOR, B_NOT, SHL, SHR,
  DOTOP, ARROW,
  ASSIGN.
  SWITCHS, CASES,
  CAST,
  COMMA,
  WHILEL, FORL, DOWHILE, 
  ADDR, DEREF, 
  FCALL, FRET, LBREAK, LGOTO, LCONT, FCOPY,
  TERNARY
} EXPTYPE;

struct expr;
struct function;
typedef struct expr{
  EXPRTYPE type;
  union{
    struct{
      int numparams;
      struct expr* params;
      struct function* func;
    };
    struct{
      struct expr* param1;
      struct expr* param2;
    }
    struct expr unaryparam;
    char* strconst;
    long intconst;
    double floatconst;
    IDENTIFIER id;
    /*possible struct const for later struct initializations*/
  };
} EXPRESSION;

typedef struct{
  char* name;
  char** fieldnames;
  VARTYPE* fieldtypes;
} STRUCT;

typedef struct{
  char* name;
  char** fieldnames;
  VARTYPE* fieldtypes;
} UNION;

typedef struct{
  char* name;
  char** fieldnames;
  int* fieldvals;
} ENUM;

typedef struct function{
  char* name;
  EXPRESSION code;
  unsigned int num_vars, num_params;
  VARTYPE retType;
} FUNC;
struct lexctx;

EXPRESSION* cloneexpr(EXPRESSION* orig){
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION param{
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->unaryparam = param;
  return retval;
}

EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION param1, EXPRESSION param2){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->param1 = param1
  retval->param2 = param2;
  return retval;
}
EXPRESSION* ct_fcall_expr(FUNCTION* func, int num, EXPRESSION* params){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  retval->func = func;
  retval->numparams = num;
  retval->params = params;
  return retval;
}
EXPRESSION* ct_strconst_expr(EXPRTYPE t, char* str){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->strconst = strdup(str);//may or may not need to duplicate
  return retval;
}
EXPRESSION* ct_intconst_expr(EXPRTYPE t, long num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
  return retval;
}
EXPRESSION* ct_floatconst_expr(EXPRTYPE t, double num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->floatconst = num;
  return retval;
}
EXPRESSION* ct_ident_expr(IDENTIFIER* id){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->id = id;
  return retval;
}

IDENTIFIER* ct_id(char* name){
  IDENTIFIER* retval = malloc(sizeof(IDENTIFIER));
  retval->name = name;//Strongly doubt that I need to duplicate
  retval->type = UNDEFINED;
  return retval;
}

#line 257 "highCs.tab.c"

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    INT = 259,
    FLOAT = 260,
    STRING_LITERAL = 261,
    ARROW = 262,
    INC = 263,
    DEC = 264,
    SHL = 265,
    SHR = 266,
    LE = 267,
    GE = 268,
    EQ = 269,
    NEQ = 270,
    AND = 271,
    OR = 272,
    DIV_GETS = 273,
    MUL_GETS = 274,
    MOD_GETS = 275,
    ADD_GETS = 276,
    SUB_GETS = 277,
    SHL_GETS = 278,
    SHR_GETS = 279,
    AND_GETS = 280,
    XOR_GETS = 281,
    OR_GETS = 282,
    TYPE_NAME = 283,
    TYPEDEF = 284,
    STATIC = 285,
    EXTERN = 286,
    CHAR = 287,
    INT8 = 288,
    INT16 = 289,
    INT32 = 290,
    INT64 = 291,
    BYTE = 292,
    DBYTE = 293,
    QBYTE = 294,
    OBYTE = 295,
    SINGLE = 296,
    DOUBLE = 297,
    CASE = 298,
    DEFAULT = 299,
    IF = 300,
    ELSE = 301,
    SWITCH = 302,
    WHILE = 303,
    DO = 304,
    FOR = 305,
    GOTO = 306,
    CONTINUE = 307,
    BREAK = 308,
    RETURN = 309,
    SIZEOF = 310,
    STRUCT = 311,
    ENUM = 312,
    UNION = 313,
    THEN = 314
  };
#endif

/* Value type.  */

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (struct lexctx* ctx);

#endif /* !YY_YY_HIGHCS_TAB_H_INCLUDED  */


/* Unqualified %code blocks.  */
#line 174 "highCs.y"

  typedef struct{
    struct{
        IDENTIFIER ident;
        char* name
    } pairs[1024];
    short index;
  } MAP;
  struct lexctx{
    //Figure out some way to have scopes here
    FUNCTION* funclist;
    unsigned int fllast, fllen;
    FUNCTION* curfunc;
    MAP* scopes[512];
    unsigned int layer;
  };
  ctx->layer = 0;
  MAP* initialscope = malloc(sizeof(MAP), 1);
  initialscope->index = 0;
  ctx->scopes[ctx->layer = 0] = initialscope;

#line 371 "highCs.tab.c"

#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#ifdef NDEBUG
# define YY_ASSERT(E) ((void) (0 && (E)))
#else
# include <assert.h> /* INFRINGES ON USER NAME SPACE */
# define YY_ASSERT(E) assert (E)
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  38
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   866

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  87
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  48
/* YYNRULES -- Number of rules.  */
#define YYNRULES  173
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  314

#define YYUNDEFTOK  2
#define YYMAXUTOK   317


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    82,     2,     2,     2,    81,    75,     2,
      63,    64,    70,    78,    62,    79,    84,    80,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    72,    60,
      76,    61,    77,    71,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    65,     2,    66,    74,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    85,    73,    86,    83,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    67,    68,    69
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   198,   198,   199,   200,   201,   203,   205,   206,   207,
     208,   210,   211,   213,   214,   215,   216,   217,   218,   220,
     221,   223,   225,   225,   225,   225,   225,   225,   225,   225,
     226,   226,   226,   226,   226,   226,   226,   226,   228,   228,
     230,   230,   230,   232,   233,   235,   236,   237,   239,   241,
     242,   244,   245,   246,   247,   249,   250,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   266,
     267,   269,   270,   272,   273,   275,   276,   278,   279,   281,
     282,   284,   285,   286,   288,   289,   290,   291,   292,   294,
     295,   296,   298,   299,   300,   302,   303,   304,   305,   307,
     308,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   322,   323,   324,   325,   326,   327,   328,   329,
     331,   332,   333,   334,   335,   337,   338,   341,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   361,   362,   364,   365,   367,
     368,   369,   370,   373,   374,   375,   377,   378,   379,   381,
     382,   384,   386,   387,   389,   390,   391,   393,   394,   395,
     397,   398,   400,   401
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "INT", "FLOAT",
  "STRING_LITERAL", "\"->\"", "\"++\"", "\"--\"", "\"<<\"", "\">>\"",
  "\"<=\"", "\">=\"", "\"==\"", "\"!=\"", "\"&&\"", "\"||\"", "\"/=\"",
  "\"*=\"", "\"%=\"", "\"+=\"", "\"-=\"", "\"<<=\"", "\">>=\"", "\"&=\"",
  "\"^=\"", "\"|=\"", "TYPE_NAME", "\"typedef\"", "\"static\"",
  "\"extern\"", "\"char\"", "\"int8\"", "\"int16\"", "\"int32\"",
  "\"int64\"", "\"byte\"", "\"dbyte\"", "\"qbyte\"", "\"obyte\"",
  "\"single\"", "\"double\"", "\"case\"", "\"default\"", "\"if\"",
  "\"else\"", "\"switch\"", "\"while\"", "\"do\"", "\"for\"", "\"goto\"",
  "\"continue\"", "\"break\"", "\"return\"", "\"sizeof\"", "\"struct\"",
  "\"enum\"", "\"union\"", "THEN", "';'", "'='", "','", "'('", "')'",
  "'['", "']'", "\"void\"", "\"const\"", "\"volatile\"", "'*'", "'?'",
  "':'", "'|'", "'^'", "'&'", "'<'", "'>'", "'+'", "'-'", "'/'", "'%'",
  "'!'", "'~'", "'.'", "'{'", "'}'", "$accept", "program", "initializer",
  "cs_inits", "declarator", "declname", "params", "param_decl", "typem",
  "types1", "types2", "typews1", "typebs", "type", "types1o",
  "abstract_ptr", "expression", "esc", "esa", "est", "eslo", "esla",
  "esbo", "esbx", "esba", "eseq", "escmp", "essh", "esas", "esm", "esca",
  "esp", "esu", "escl", "function", "statement", "ee",
  "compound_statement", "statements_and_initializers", "union", "struct",
  "struct_decls", "struct_decl", "cs_decls", "sdecl", "enum", "enums",
  "enumerator", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
      59,    61,    44,    40,    41,    91,    93,   315,   316,   317,
      42,    63,    58,   124,    94,    38,    60,    62,    43,    45,
      47,    37,    33,   126,    46,   123,   125
};
# endif

#define YYPACT_NINF (-170)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     759,  -170,  -170,  -170,  -170,  -170,  -170,  -170,  -170,  -170,
    -170,  -170,  -170,  -170,  -170,  -170,    18,    19,    20,  -170,
    -170,  -170,    71,   759,  -170,   759,   759,     6,   759,  -170,
    -170,  -170,   -67,   797,   -56,    76,     1,   797,  -170,  -170,
    -170,  -170,  -170,     6,    68,    50,   -22,     3,    39,  -170,
     797,  -170,   797,  -170,    12,   537,  -170,    76,    57,   -51,
    -170,   797,   599,    89,  -170,    68,  -170,  -170,   465,     6,
     210,  -170,   717,   117,     3,   637,  -170,   465,    86,    85,
    -170,  -170,  -170,   -48,   465,    76,  -170,   675,  -170,  -170,
    -170,  -170,  -170,  -170,  -170,  -170,   533,   533,   547,   409,
     465,   465,   465,   465,   465,   465,   361,  -170,    -7,   150,
     113,    96,   115,   147,    35,   104,    99,    47,  -170,  -170,
     100,  -170,  -170,   120,   133,   465,   145,   159,   160,   164,
     346,   165,   208,   169,   170,   430,  -170,  -170,  -170,     6,
      97,   625,  -170,  -170,   278,  -170,   168,   171,     6,  -170,
     -17,  -170,   625,   465,  -170,    12,  -170,   625,  -170,  -170,
     465,  -170,  -170,   409,  -170,   172,   111,  -170,  -170,  -170,
    -170,  -170,  -170,   465,   465,   465,   465,   465,   465,   465,
     465,   465,   465,   465,     6,   465,   465,   465,   465,   465,
     465,   465,   465,   465,   465,   465,   465,   465,   465,   465,
     465,   465,   465,   465,   231,  -170,  -170,   451,   465,   253,
     346,   347,   346,   465,   465,   465,   221,   465,   211,  -170,
    -170,  -170,   114,  -170,   465,  -170,  -170,  -170,  -170,   759,
    -170,  -170,   625,  -170,   212,   465,  -170,  -170,  -170,  -170,
    -170,  -170,  -170,  -170,  -170,  -170,  -170,  -170,  -170,   150,
     -21,   113,    96,   115,   147,    35,    35,   104,   104,   104,
     104,    99,    99,    47,    47,  -170,  -170,  -170,  -170,  -170,
     625,   127,    11,  -170,  -170,   346,  -170,   139,   140,   144,
     209,   213,   214,  -170,  -170,   625,  -170,  -170,  -170,   465,
     465,  -170,  -170,  -170,   346,   346,   346,   465,   465,  -170,
     625,   244,  -170,  -170,   148,   234,   346,   237,   465,  -170,
    -170,   227,   346,  -170
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    37,    40,    42,    41,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,     0,     0,     0,    33,
      38,    39,     0,     3,    45,     0,     0,     0,     2,    36,
      35,    34,   158,     0,   169,     0,   155,     0,     1,     5,
      46,    47,    13,     0,    51,     0,    10,    12,     0,     4,
       0,    43,     0,    48,     0,     0,   159,     0,   172,     0,
     170,     0,     0,     0,    49,    53,    52,     6,     0,     0,
       0,   127,     0,     0,    11,     0,    44,     0,   164,     0,
     163,   157,   160,     0,     0,     0,   168,     0,   154,    14,
      50,    54,   124,   122,   123,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     8,    68,    70,    72,
      74,    76,    78,    80,    83,    88,    91,    94,    98,   100,
     111,   119,     9,    10,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   144,   147,   149,     0,
       0,    56,   150,   128,     0,    17,     0,    19,     0,    15,
       0,   156,   166,     0,   161,     0,   167,   173,   171,   153,
       0,   101,   102,     0,   110,     0,     0,   107,   108,   103,
     104,   105,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   146,     0,   140,
     139,   141,     0,   143,     0,   148,   151,   152,    18,     0,
      21,    16,   165,   162,     0,     0,   120,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    57,     7,    71,
       0,    73,    75,    77,    79,    81,    82,    86,    87,    84,
      85,    89,    90,    92,    93,    95,    96,    97,   118,   114,
     125,     0,     0,   117,   129,     0,   131,     0,     0,     0,
       0,   145,     0,   138,   142,    55,    20,   109,    99,     0,
       0,   115,   116,   130,     0,     0,     0,     0,   146,    69,
     126,   132,   134,   135,     0,     0,     0,     0,   146,   133,
     136,     0,     0,   137
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -170,    37,   -63,   -68,    16,   250,    70,  -170,     8,     2,
    -170,   248,   -20,   -75,  -170,   -28,   -73,   -64,   -82,  -170,
     116,   118,   136,   151,   112,    29,   -62,    -4,    -2,   -47,
      53,  -170,  -170,  -170,  -170,  -127,  -169,   257,  -170,  -170,
    -170,    69,   -43,  -170,   149,  -170,   280,   254
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    22,    23,    45,   123,    47,   146,   147,    51,    52,
      26,    53,    27,    54,    65,    48,   140,   141,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   271,    28,   142,   282,   143,   144,    29,
      30,    55,    56,    79,    80,    31,    59,    60
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     150,   122,    25,   216,   106,    40,    41,   138,    24,    42,
     185,    85,    82,   152,    85,    42,    66,   227,    50,    82,
     157,    32,    34,    36,   165,    25,   166,    25,    25,    57,
      25,    24,    82,    24,    24,    86,    24,    91,   156,    68,
      69,   224,    42,    46,    82,   224,    64,   193,   194,   231,
     139,   289,   148,   167,   168,   169,   170,   171,   172,    63,
      39,   211,   222,    70,   186,    49,    72,    90,    73,    43,
      78,    38,    25,   224,    25,    43,    44,   292,    24,    58,
      24,   226,    44,   274,    77,   276,    61,   166,   234,   232,
     166,   237,   238,   239,   240,   241,   242,   243,   244,   245,
     246,   247,    43,    33,    35,    37,    62,   204,   205,   206,
      67,   195,   196,   250,   197,   198,   248,   201,    84,    75,
      92,    93,    94,    95,   139,    96,    97,   202,   203,   305,
      87,   257,   258,   259,   260,   272,    20,    21,    44,   311,
     277,   278,   279,   270,   281,   154,    25,   155,   293,   161,
     162,   164,    24,    89,   265,   266,   267,   223,   153,   224,
     285,   191,   192,   207,   230,   208,   187,   301,   302,   303,
     189,    78,    98,   224,   284,   236,   224,   199,   200,   309,
      99,    68,    69,   149,   209,   313,   188,   100,   288,   290,
     190,   291,   101,   261,   262,   102,   103,   263,   264,   104,
     105,   224,   224,   294,   295,   210,   224,   299,   296,   148,
     224,   218,   307,   124,    93,    94,    95,   212,    96,    97,
     255,   256,   213,   214,   304,   281,   300,   215,   217,   219,
     220,    25,   228,   229,   268,   281,   235,    24,     1,     2,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,   125,   126,   127,   273,   128,   129,   130,
     131,   132,   133,   134,   135,    98,    16,    17,    18,   280,
     136,   283,   297,    99,   298,   224,   287,    19,    20,    21,
     100,   124,    93,    94,    95,   101,    96,    97,   102,   103,
     306,   312,   104,   105,   308,    70,   137,   310,    74,   286,
      76,   249,   254,    71,   233,   251,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,   125,   126,   127,   252,   128,   129,   130,   131,   132,
     133,   134,   135,    98,    16,    17,    18,    83,   136,   158,
     253,    99,     0,     0,     0,    19,    20,    21,   100,   124,
      93,    94,    95,   101,    96,    97,   102,   103,     0,     0,
     104,   105,     0,    70,   225,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   125,
     126,   127,     0,   128,   129,   130,   131,   132,   133,   134,
     135,    98,     0,     0,     0,     0,   136,     0,   183,    99,
       0,     0,    92,    93,    94,    95,   100,    96,    97,   275,
       0,   101,   183,   184,   102,   103,     0,     0,   104,   105,
       0,    70,     0,    92,    93,    94,    95,     1,    96,    97,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,     0,     0,    92,    93,    94,    95,     0,    96,
      97,     0,     0,     0,    98,    16,    17,    18,    92,    93,
      94,    95,    99,    96,    97,     0,    19,    20,    21,   100,
       0,     0,     0,     0,   101,    98,     0,   102,   103,     0,
     221,   104,   105,    99,     0,     0,     0,     0,     0,     0,
     100,     0,     0,     0,     0,   101,    98,     0,   102,   103,
       0,     0,   104,   105,    99,   269,     0,     0,     0,     0,
      98,   100,     0,     0,     0,     0,   101,     0,    99,   102,
     103,     0,     0,   104,   105,   100,    92,    93,    94,    95,
     101,    96,    97,   102,   103,     0,     0,   104,   105,     0,
      92,    93,    94,    95,     0,    96,    97,     0,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
       0,     0,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,     0,    16,    17,    18,   160,     0,     0,     0,
       0,     0,    98,   100,    19,    20,    21,     0,   101,     0,
     163,   102,   103,     0,     0,   104,   105,   100,     0,     0,
       0,     0,   101,    81,     0,   102,   103,     1,     0,   104,
     105,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     1,    19,    20,    21,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
       0,     0,     0,     0,     0,    88,   183,     0,     0,     0,
       0,     0,     0,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     1,    19,    20,    21,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,     0,     0,
       0,     0,     0,   151,     0,     0,     0,     0,     0,     0,
       0,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,    21,     1,     2,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
       0,   159,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    16,    17,    18,     0,     0,     0,     0,
       0,   145,     0,     0,    19,    20,    21,     1,     2,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     1,    19,    20,    21,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,    19,    20,    21
};

static const yytype_int16 yycheck[] =
{
      73,    69,     0,   130,    68,    25,    26,    70,     0,     3,
      17,    62,    55,    77,    62,     3,    44,   144,    85,    62,
      84,     3,     3,     3,    99,    23,    99,    25,    26,    85,
      28,    23,    75,    25,    26,    86,    28,    65,    86,    61,
      62,    62,     3,    27,    87,    62,    44,    12,    13,    66,
      70,    72,    72,   100,   101,   102,   103,   104,   105,    43,
      23,   125,   135,    85,    71,    28,    63,    65,    65,    63,
      54,     0,    70,    62,    72,    63,    70,    66,    70,     3,
      72,   144,    70,   210,    72,   212,    85,   160,   163,   153,
     163,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,   183,    63,    85,    85,    85,    37,     7,     8,     9,
      60,    76,    77,   186,    10,    11,   184,    70,    61,    50,
       3,     4,     5,     6,   144,     8,     9,    80,    81,   298,
      61,   193,   194,   195,   196,   208,    68,    69,    70,   308,
     213,   214,   215,   207,   217,    60,   144,    62,   275,    96,
      97,    98,   144,    64,   201,   202,   203,    60,    72,    62,
     224,    14,    15,    63,   148,    65,    16,   294,   295,   296,
      74,   155,    55,    62,    60,    64,    62,    78,    79,   306,
      63,    61,    62,    66,    84,   312,    73,    70,   235,    62,
      75,    64,    75,   197,   198,    78,    79,   199,   200,    82,
      83,    62,    62,    64,    64,    72,    62,   289,    64,   229,
      62,     3,    64,     3,     4,     5,     6,    72,     8,     9,
     191,   192,    63,    63,   297,   298,   290,    63,    63,    60,
      60,   229,    64,    62,     3,   308,    64,   229,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     3,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    48,
      60,    60,    63,    63,    60,    62,    64,    67,    68,    69,
      70,     3,     4,     5,     6,    75,     8,     9,    78,    79,
      46,    64,    82,    83,    60,    85,    86,    60,    48,   229,
      52,   185,   190,    46,   155,   187,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   188,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    57,    60,    85,
     189,    63,    -1,    -1,    -1,    67,    68,    69,    70,     3,
       4,     5,     6,    75,     8,     9,    78,    79,    -1,    -1,
      82,    83,    -1,    85,    86,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    -1,    -1,    -1,    -1,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    43,
      44,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    -1,    -1,    -1,    60,    -1,    61,    63,
      -1,    -1,     3,     4,     5,     6,    70,     8,     9,    72,
      -1,    75,    61,    62,    78,    79,    -1,    -1,    82,    83,
      -1,    85,    -1,     3,     4,     5,     6,    28,     8,     9,
      -1,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,     3,     4,     5,     6,    -1,     8,
       9,    -1,    -1,    -1,    55,    56,    57,    58,     3,     4,
       5,     6,    63,     8,     9,    -1,    67,    68,    69,    70,
      -1,    -1,    -1,    -1,    75,    55,    -1,    78,    79,    -1,
      60,    82,    83,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    75,    55,    -1,    78,    79,
      -1,    -1,    82,    83,    63,    64,    -1,    -1,    -1,    -1,
      55,    70,    -1,    -1,    -1,    -1,    75,    -1,    63,    78,
      79,    -1,    -1,    82,    83,    70,     3,     4,     5,     6,
      75,     8,     9,    78,    79,    -1,    -1,    82,    83,    -1,
       3,     4,     5,     6,    -1,     8,     9,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    -1,    -1,    -1,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    -1,    -1,    56,    57,    58,    63,    -1,    -1,    -1,
      -1,    -1,    55,    70,    67,    68,    69,    -1,    75,    -1,
      63,    78,    79,    -1,    -1,    82,    83,    70,    -1,    -1,
      -1,    -1,    75,    86,    -1,    78,    79,    28,    -1,    82,
      83,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    -1,    -1,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    67,    68,    69,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    86,    61,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    67,    68,    69,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    69,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    67,    68,    69,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    67,    68,    69,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    69
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    56,    57,    58,    67,
      68,    69,    88,    89,    95,    96,    97,    99,   121,   126,
     127,   132,     3,    85,     3,    85,     3,    85,     0,    88,
      99,    99,     3,    63,    70,    90,    91,    92,   102,    88,
      85,    95,    96,    98,   100,   128,   129,    85,     3,   133,
     134,    85,   128,    91,    96,   101,   102,    60,    61,    62,
      85,   124,    63,    65,    92,   128,    98,    72,    91,   130,
     131,    86,   129,   133,    61,    62,    86,   128,    86,    64,
      96,   102,     3,     4,     5,     6,     8,     9,    55,    63,
      70,    75,    78,    79,    82,    83,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    90,    91,     3,    43,    44,    45,    47,    48,
      49,    50,    51,    52,    53,    54,    60,    86,    89,    99,
     103,   104,   122,   124,   125,    64,    93,    94,    99,    66,
     103,    86,   104,    72,    60,    62,    86,   104,   134,    86,
      63,   117,   117,    63,   117,   100,   103,   116,   116,   116,
     116,   116,   116,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    61,    62,    17,    71,    16,    73,    74,
      75,    14,    15,    12,    13,    76,    77,    10,    11,    78,
      79,    70,    80,    81,     7,     8,     9,    63,    65,    84,
      72,   104,    72,    63,    63,    63,   122,    63,     3,    60,
      60,    60,   103,    60,    62,    86,    89,   122,    64,    62,
      91,    66,   104,   131,   100,    64,    64,   105,   105,   105,
     105,   105,   105,   105,   105,   105,   105,   105,    90,   107,
     103,   108,   109,   110,   111,   112,   112,   113,   113,   113,
     113,   114,   114,   115,   115,   116,   116,   116,     3,    64,
     104,   120,   103,     3,   122,    72,   122,   103,   103,   103,
      48,   103,   123,    60,    60,   104,    93,    64,   116,    72,
      62,    64,    66,   122,    64,    64,    64,    63,    60,   105,
     104,   122,   122,   122,   103,   123,    46,    64,    60,   122,
      60,   123,    64,   122
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    87,    88,    88,    88,    88,    89,    90,    90,    90,
      90,    91,    91,    92,    92,    92,    92,    92,    92,    93,
      93,    94,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    96,    96,
      97,    97,    97,    98,    98,    99,    99,    99,   100,   101,
     101,   102,   102,   102,   102,   103,   103,   104,   104,   104,
     104,   104,   104,   104,   104,   104,   104,   104,   104,   105,
     105,   106,   106,   107,   107,   108,   108,   109,   109,   110,
     110,   111,   111,   111,   112,   112,   112,   112,   112,   113,
     113,   113,   114,   114,   114,   115,   115,   115,   115,   116,
     116,   117,   117,   117,   117,   117,   117,   117,   117,   117,
     117,   117,   118,   118,   118,   118,   118,   118,   118,   118,
     119,   119,   119,   119,   119,   120,   120,   121,   122,   122,
     122,   122,   122,   122,   122,   122,   122,   122,   122,   122,
     122,   122,   122,   122,   122,   123,   123,   124,   124,   125,
     125,   125,   125,   126,   126,   126,   127,   127,   127,   128,
     128,   129,   130,   130,   131,   131,   131,   132,   132,   132,
     133,   133,   134,   134
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     3,     5,     3,     3,
       1,     2,     1,     1,     3,     3,     4,     3,     4,     1,
       3,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     2,     2,     1,     1,
       2,     1,     2,     2,     3,     3,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     1,     5,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       3,     1,     3,     3,     1,     3,     3,     3,     1,     4,
       1,     2,     2,     2,     2,     2,     2,     2,     2,     4,
       2,     1,     2,     2,     3,     4,     4,     3,     3,     1,
       3,     1,     1,     1,     1,     1,     3,     3,     1,     3,
       4,     3,     5,     7,     5,     5,     7,     9,     3,     2,
       2,     2,     3,     2,     1,     1,     0,     2,     3,     1,
       1,     2,     2,     5,     4,     2,     5,     4,     2,     1,
       2,     3,     3,     1,     1,     3,     2,     5,     4,     2,
       1,     3,     1,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (ctx, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, ctx); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct lexctx* ctx)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  YYUSE (ctx);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct lexctx* ctx)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp, ctx);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, struct lexctx* ctx)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , ctx);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, ctx); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct lexctx* ctx)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (ctx);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct lexctx* ctx)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (ctx);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 55:
#line 249 "highCs.y"
                     {(yyval.EXPRESSION*) = ct_binary_expr(COMMA, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 1982 "highCs.tab.c"
    break;

  case 56:
#line 250 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 1988 "highCs.tab.c"
    break;

  case 57:
#line 252 "highCs.y"
              {(yyval.EXPRESSION*) = ct_binary_expr(ASSIGN, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 1994 "highCs.tab.c"
    break;

  case 68:
#line 263 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2000 "highCs.tab.c"
    break;

  case 70:
#line 267 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2006 "highCs.tab.c"
    break;

  case 71:
#line 269 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(L_OR, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2012 "highCs.tab.c"
    break;

  case 72:
#line 270 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2018 "highCs.tab.c"
    break;

  case 73:
#line 272 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_binary_expr(L_AND, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2024 "highCs.tab.c"
    break;

  case 74:
#line 273 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2030 "highCs.tab.c"
    break;

  case 75:
#line 275 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(B_OR, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2036 "highCs.tab.c"
    break;

  case 76:
#line 276 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2042 "highCs.tab.c"
    break;

  case 77:
#line 278 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(B_XOR, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2048 "highCs.tab.c"
    break;

  case 78:
#line 279 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2054 "highCs.tab.c"
    break;

  case 79:
#line 281 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(B_AND, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2060 "highCs.tab.c"
    break;

  case 80:
#line 282 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2066 "highCs.tab.c"
    break;

  case 81:
#line 284 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_binary_expr(EQ, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2072 "highCs.tab.c"
    break;

  case 82:
#line 285 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_binary_expr(NEQ, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2078 "highCs.tab.c"
    break;

  case 83:
#line 286 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2084 "highCs.tab.c"
    break;

  case 84:
#line 288 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_binary_expr(LT, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2090 "highCs.tab.c"
    break;

  case 85:
#line 289 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_binary_expr(GT, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2096 "highCs.tab.c"
    break;

  case 86:
#line 290 "highCs.y"
                  {(yyval.EXPRESSION*) = ct_binary_expr(LTE, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2102 "highCs.tab.c"
    break;

  case 87:
#line 291 "highCs.y"
                  {(yyval.EXPRESSION*) = ct_binary_expr(GTE, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2108 "highCs.tab.c"
    break;

  case 88:
#line 292 "highCs.y"
        {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2114 "highCs.tab.c"
    break;

  case 89:
#line 294 "highCs.y"
                  {(yyval.EXPRESSION*) = ct_binary_expr(SHL, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2120 "highCs.tab.c"
    break;

  case 90:
#line 295 "highCs.y"
                  {(yyval.EXPRESSION*) = ct_binary_expr(SHR, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2126 "highCs.tab.c"
    break;

  case 92:
#line 298 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(ADD, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2132 "highCs.tab.c"
    break;

  case 93:
#line 299 "highCs.y"
                {(yyval.EXPRESSION*) = ct_binary_expr(SUB, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2138 "highCs.tab.c"
    break;

  case 94:
#line 300 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2144 "highCs.tab.c"
    break;

  case 95:
#line 302 "highCs.y"
               {(yyval.EXPRESSION*) = ct_binary_expr(MULT, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2150 "highCs.tab.c"
    break;

  case 96:
#line 303 "highCs.y"
               {(yyval.EXPRESSION*) = ct_binary_expr(DIVI, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2156 "highCs.tab.c"
    break;

  case 97:
#line 304 "highCs.y"
               {(yyval.EXPRESSION*) = ct_binary_expr(MOD, (yyvsp[-2].EXPRESSION*), (yyvsp[0].EXPRESSION*));}
#line 2162 "highCs.tab.c"
    break;

  case 98:
#line 305 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2168 "highCs.tab.c"
    break;

  case 99:
#line 307 "highCs.y"
                   {/*$$ = coerce_type($2, $4);*/}
#line 2174 "highCs.tab.c"
    break;

  case 100:
#line 308 "highCs.y"
       {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2180 "highCs.tab.c"
    break;

  case 101:
#line 310 "highCs.y"
            {(yyval.EXPRESSION*) = ct_unary_expr(PREINC, (yyvsp[0].EXPRESSION*));}
#line 2186 "highCs.tab.c"
    break;

  case 102:
#line 311 "highCs.y"
            {(yyval.EXPRESSION*) = ct_unary_expr(PREDEC, (yyvsp[0].EXPRESSION*));}
#line 2192 "highCs.tab.c"
    break;

  case 103:
#line 312 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(IDENT, (yyvsp[0].EXPRESSION*));}
#line 2198 "highCs.tab.c"
    break;

  case 104:
#line 313 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(NEG, (yyvsp[0].EXPRESSION*));}
#line 2204 "highCs.tab.c"
    break;

  case 105:
#line 314 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(L_NOT, (yyvsp[0].EXPRESSION*));}
#line 2210 "highCs.tab.c"
    break;

  case 106:
#line 315 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(B_NOT, (yyvsp[0].EXPRESSION*));}
#line 2216 "highCs.tab.c"
    break;

  case 107:
#line 316 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(DEREF, (yyvsp[0].EXPRESSION*));}
#line 2222 "highCs.tab.c"
    break;

  case 108:
#line 317 "highCs.y"
          {(yyval.EXPRESSION*) = ct_unary_expr(ADDR, (yyvsp[0].EXPRESSION*));}
#line 2228 "highCs.tab.c"
    break;

  case 109:
#line 318 "highCs.y"
                        {/*$$ = ct_intconst_expr(szof($3));*/}
#line 2234 "highCs.tab.c"
    break;

  case 110:
#line 319 "highCs.y"
                {(yyval.EXPRESSION*) = ct_intconst_expr(szofexpr((yyvsp[0].EXPRESSION*)));}
#line 2240 "highCs.tab.c"
    break;

  case 111:
#line 320 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2246 "highCs.tab.c"
    break;

  case 112:
#line 322 "highCs.y"
           {(yyval.EXPRESSION*) = ct_unary_expr(POSTINC, (yyvsp[-1].EXPRESSION*));}
#line 2252 "highCs.tab.c"
    break;

  case 113:
#line 323 "highCs.y"
           {(yyval.EXPRESSION*) = ct_unary_expr(POSTDEC, (yyvsp[-1].EXPRESSION*));}
#line 2258 "highCs.tab.c"
    break;

  case 114:
#line 324 "highCs.y"
              {(yyval.EXPRESSION*) = ct_nary_expr((yyvsp[-2].EXPRESSION*),0,NULL);}
#line 2264 "highCs.tab.c"
    break;

  case 115:
#line 325 "highCs.y"
                   {(yyval.EXPRESSION*) = ct_nary_expr((yyvsp[-3].EXPRESSION*),,NULL);}
#line 2270 "highCs.tab.c"
    break;

  case 116:
#line 326 "highCs.y"
                         {(yyval.EXPRESSION*) = ct_unary_expr(DEREF, ct_binary_expr(ADD, (yyvsp[-3].EXPRESSION*), (yyvsp[-1].EXPRESSION*)));}
#line 2276 "highCs.tab.c"
    break;

  case 117:
#line 327 "highCs.y"
                     {(yyval.EXPRESSION*) = ct_binary_expr(DOTOP, (yyvsp[-2].EXPRESSION*), ct_ident_expr((yyvsp[0].IDENTIFIER*)));}
#line 2282 "highCs.tab.c"
    break;

  case 118:
#line 328 "highCs.y"
                       {(yyval.EXPRESSION*) = ct_binary_expr(ARROW, (yyvsp[-2].EXPRESSION*), ct_ident_expr((yyvsp[0].IDENTIFIER*)));}
#line 2288 "highCs.tab.c"
    break;

  case 119:
#line 329 "highCs.y"
      {(yyval.EXPRESSION*) = (yyvsp[0].EXPRESSION*)}
#line 2294 "highCs.tab.c"
    break;

  case 120:
#line 331 "highCs.y"
                     {(yyval.EXPRESSION*) = (yyvsp[-1].EXPRESSION*);}
#line 2300 "highCs.tab.c"
    break;

  case 121:
#line 332 "highCs.y"
                 {(yyval.EXPRESSION*) = ct_strconst_expr((yyvsp[0].char*));}
#line 2306 "highCs.tab.c"
    break;

  case 122:
#line 333 "highCs.y"
      {(yyval.EXPRESSION*) = ct_intconst_expr((yyvsp[0].long));}
#line 2312 "highCs.tab.c"
    break;

  case 123:
#line 334 "highCs.y"
        {(yyval.EXPRESSION*) = ct_floatconst_expr((yyvsp[0].double));}
#line 2318 "highCs.tab.c"
    break;

  case 124:
#line 335 "highCs.y"
             {(yyval.EXPRESSION*) = ct_ident_expr((yyvsp[0].IDENTIFIER*));}
#line 2324 "highCs.tab.c"
    break;

  case 125:
#line 337 "highCs.y"
      {/*$$ = $1*/}
#line 2330 "highCs.tab.c"
    break;


#line 2334 "highCs.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (ctx, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (ctx, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, ctx);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp, ctx);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (ctx, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, ctx);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, ctx);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 404 "highCs.y"

#include <stdio.h>
int yyerror(char* s){
    printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
}

