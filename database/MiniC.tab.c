/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "frontend/flexbison/MiniC.y"

#include <cstdio>
#include <cstring>

// 词法分析头文件
#include "FlexLexer.h"

// bison生成的头文件
#include "BisonParser.h"

// 抽象语法树函数定义原型头文件
#include "AST.h"

#include "IntegerType.h"

// LR分析失败时所调用函数的原型声明
void yyerror(char * msg);


#line 91 "MiniC.tab.c"

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


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    T_DIGIT = 258,                 /* T_DIGIT  */
    T_ID = 259,                    /* T_ID  */
    T_INT = 260,                   /* T_INT  */
    T_VOID = 261,                  /* T_VOID  */
    T_L_BRACKET = 262,             /* T_L_BRACKET  */
    T_R_BRACKET = 263,             /* T_R_BRACKET  */
    T_RETURN = 264,                /* T_RETURN  */
    T_SEMICOLON = 265,             /* T_SEMICOLON  */
    T_L_PAREN = 266,               /* T_L_PAREN  */
    T_R_PAREN = 267,               /* T_R_PAREN  */
    T_L_BRACE = 268,               /* T_L_BRACE  */
    T_R_BRACE = 269,               /* T_R_BRACE  */
    T_COMMA = 270,                 /* T_COMMA  */
    T_ASSIGN = 271,                /* T_ASSIGN  */
    T_SUB = 272,                   /* T_SUB  */
    T_ADD = 273,                   /* T_ADD  */
    T_MUL = 274,                   /* T_MUL  */
    T_DIV = 275,                   /* T_DIV  */
    T_MOD = 276,                   /* T_MOD  */
    T_LESS = 277,                  /* T_LESS  */
    T_GREATER = 278,               /* T_GREATER  */
    T_LESS_EQUAL = 279,            /* T_LESS_EQUAL  */
    T_GREATER_EQUAL = 280,         /* T_GREATER_EQUAL  */
    T_EQUAL = 281,                 /* T_EQUAL  */
    T_NOT_EQUAL = 282,             /* T_NOT_EQUAL  */
    T_AND_AND = 283,               /* T_AND_AND  */
    T_OR_OR = 284,                 /* T_OR_OR  */
    T_NOT = 285,                   /* T_NOT  */
    T_IF = 286,                    /* T_IF  */
    T_ELSE = 287,                  /* T_ELSE  */
    T_WHILE = 288,                 /* T_WHILE  */
    T_FOR = 289,                   /* T_FOR  */
    T_BREAK = 290,                 /* T_BREAK  */
    T_CONTINUE = 291,              /* T_CONTINUE  */
    T_INC = 292,                   /* T_INC  */
    T_DEC = 293,                   /* T_DEC  */
    LOWER_THAN_ELSE = 294          /* LOWER_THAN_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 22 "frontend/flexbison/MiniC.y"

    class ast_node * node;

    struct digit_int_attr integer_num;
    struct digit_real_attr float_num;
    struct var_id_attr var_id;
    struct type_attr type;
    int op_class;

	// 数组维度和下标链表类型
    struct array_dim_node * array_dims;
    struct array_index_node * array_indices;

#line 191 "MiniC.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_T_DIGIT = 3,                    /* T_DIGIT  */
  YYSYMBOL_T_ID = 4,                       /* T_ID  */
  YYSYMBOL_T_INT = 5,                      /* T_INT  */
  YYSYMBOL_T_VOID = 6,                     /* T_VOID  */
  YYSYMBOL_T_L_BRACKET = 7,                /* T_L_BRACKET  */
  YYSYMBOL_T_R_BRACKET = 8,                /* T_R_BRACKET  */
  YYSYMBOL_T_RETURN = 9,                   /* T_RETURN  */
  YYSYMBOL_T_SEMICOLON = 10,               /* T_SEMICOLON  */
  YYSYMBOL_T_L_PAREN = 11,                 /* T_L_PAREN  */
  YYSYMBOL_T_R_PAREN = 12,                 /* T_R_PAREN  */
  YYSYMBOL_T_L_BRACE = 13,                 /* T_L_BRACE  */
  YYSYMBOL_T_R_BRACE = 14,                 /* T_R_BRACE  */
  YYSYMBOL_T_COMMA = 15,                   /* T_COMMA  */
  YYSYMBOL_T_ASSIGN = 16,                  /* T_ASSIGN  */
  YYSYMBOL_T_SUB = 17,                     /* T_SUB  */
  YYSYMBOL_T_ADD = 18,                     /* T_ADD  */
  YYSYMBOL_T_MUL = 19,                     /* T_MUL  */
  YYSYMBOL_T_DIV = 20,                     /* T_DIV  */
  YYSYMBOL_T_MOD = 21,                     /* T_MOD  */
  YYSYMBOL_T_LESS = 22,                    /* T_LESS  */
  YYSYMBOL_T_GREATER = 23,                 /* T_GREATER  */
  YYSYMBOL_T_LESS_EQUAL = 24,              /* T_LESS_EQUAL  */
  YYSYMBOL_T_GREATER_EQUAL = 25,           /* T_GREATER_EQUAL  */
  YYSYMBOL_T_EQUAL = 26,                   /* T_EQUAL  */
  YYSYMBOL_T_NOT_EQUAL = 27,               /* T_NOT_EQUAL  */
  YYSYMBOL_T_AND_AND = 28,                 /* T_AND_AND  */
  YYSYMBOL_T_OR_OR = 29,                   /* T_OR_OR  */
  YYSYMBOL_T_NOT = 30,                     /* T_NOT  */
  YYSYMBOL_T_IF = 31,                      /* T_IF  */
  YYSYMBOL_T_ELSE = 32,                    /* T_ELSE  */
  YYSYMBOL_T_WHILE = 33,                   /* T_WHILE  */
  YYSYMBOL_T_FOR = 34,                     /* T_FOR  */
  YYSYMBOL_T_BREAK = 35,                   /* T_BREAK  */
  YYSYMBOL_T_CONTINUE = 36,                /* T_CONTINUE  */
  YYSYMBOL_T_INC = 37,                     /* T_INC  */
  YYSYMBOL_T_DEC = 38,                     /* T_DEC  */
  YYSYMBOL_LOWER_THAN_ELSE = 39,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_YYACCEPT = 40,                  /* $accept  */
  YYSYMBOL_CompileUnit = 41,               /* CompileUnit  */
  YYSYMBOL_FuncDef = 42,                   /* FuncDef  */
  YYSYMBOL_Block = 43,                     /* Block  */
  YYSYMBOL_BlockItemList = 44,             /* BlockItemList  */
  YYSYMBOL_BlockItem = 45,                 /* BlockItem  */
  YYSYMBOL_VarDecl = 46,                   /* VarDecl  */
  YYSYMBOL_VarDeclExpr = 47,               /* VarDeclExpr  */
  YYSYMBOL_VarDef = 48,                    /* VarDef  */
  YYSYMBOL_ArrayDims = 49,                 /* ArrayDims  */
  YYSYMBOL_InitVal = 50,                   /* InitVal  */
  YYSYMBOL_InitValList = 51,               /* InitValList  */
  YYSYMBOL_BasicType = 52,                 /* BasicType  */
  YYSYMBOL_Statement = 53,                 /* Statement  */
  YYSYMBOL_ForInitStmt = 54,               /* ForInitStmt  */
  YYSYMBOL_ForCondExpr = 55,               /* ForCondExpr  */
  YYSYMBOL_ForStepExpr = 56,               /* ForStepExpr  */
  YYSYMBOL_Expr = 57,                      /* Expr  */
  YYSYMBOL_LOrExp = 58,                    /* LOrExp  */
  YYSYMBOL_LAndExp = 59,                   /* LAndExp  */
  YYSYMBOL_EqExp = 60,                     /* EqExp  */
  YYSYMBOL_RelExp = 61,                    /* RelExp  */
  YYSYMBOL_AddExp = 62,                    /* AddExp  */
  YYSYMBOL_MulExp = 63,                    /* MulExp  */
  YYSYMBOL_UnaryExp = 64,                  /* UnaryExp  */
  YYSYMBOL_PrimaryExp = 65,                /* PrimaryExp  */
  YYSYMBOL_RealParamList = 66,             /* RealParamList  */
  YYSYMBOL_FormalParam = 67,               /* FormalParam  */
  YYSYMBOL_ArrayParamDims = 68,            /* ArrayParamDims  */
  YYSYMBOL_FormalParamList = 69,           /* FormalParamList  */
  YYSYMBOL_LVal = 70,                      /* LVal  */
  YYSYMBOL_ArrayIndices = 71               /* ArrayIndices  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




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

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
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
typedef yytype_uint8 yy_state_t;

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
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
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


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

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
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   313

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  40
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  32
/* YYNRULES -- Number of rules.  */
#define YYNRULES  95
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  174

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   294


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   155,   155,   163,   169,   174,   181,   199,   221,   227,
     238,   244,   254,   258,   269,   275,   287,   301,   311,   319,
     325,   336,   340,   347,   350,   357,   360,   366,   369,   378,
     384,   389,   395,   401,   407,   413,   419,   425,   431,   436,
     442,   450,   454,   457,   463,   467,   473,   477,   485,   489,
     496,   500,   507,   511,   525,   529,   533,   540,   544,   548,
     552,   556,   565,   568,   571,   578,   581,   584,   587,   597,
     603,   619,   634,   638,   642,   646,   650,   654,   658,   667,
     671,   677,   708,   712,   718,   724,   739,   743,   746,   749,
     756,   760,   767,   773,   786,   790
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "T_DIGIT", "T_ID",
  "T_INT", "T_VOID", "T_L_BRACKET", "T_R_BRACKET", "T_RETURN",
  "T_SEMICOLON", "T_L_PAREN", "T_R_PAREN", "T_L_BRACE", "T_R_BRACE",
  "T_COMMA", "T_ASSIGN", "T_SUB", "T_ADD", "T_MUL", "T_DIV", "T_MOD",
  "T_LESS", "T_GREATER", "T_LESS_EQUAL", "T_GREATER_EQUAL", "T_EQUAL",
  "T_NOT_EQUAL", "T_AND_AND", "T_OR_OR", "T_NOT", "T_IF", "T_ELSE",
  "T_WHILE", "T_FOR", "T_BREAK", "T_CONTINUE", "T_INC", "T_DEC",
  "LOWER_THAN_ELSE", "$accept", "CompileUnit", "FuncDef", "Block",
  "BlockItemList", "BlockItem", "VarDecl", "VarDeclExpr", "VarDef",
  "ArrayDims", "InitVal", "InitValList", "BasicType", "Statement",
  "ForInitStmt", "ForCondExpr", "ForStepExpr", "Expr", "LOrExp", "LAndExp",
  "EqExp", "RelExp", "AddExp", "MulExp", "UnaryExp", "PrimaryExp",
  "RealParamList", "FormalParam", "ArrayParamDims", "FormalParamList",
  "LVal", "ArrayIndices", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-137)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      58,  -137,  -137,    83,  -137,  -137,    -6,    20,  -137,  -137,
    -137,  -137,    31,    43,  -137,    -4,  -137,   275,    55,   275,
      27,  -137,    37,   275,   275,   275,   275,   275,   275,    39,
      22,    59,    85,    68,   100,    86,    82,  -137,    97,    90,
      96,  -137,    34,  -137,   275,    91,   275,   211,   116,   114,
      82,  -137,    82,    82,    82,    82,  -137,   275,   275,   275,
     275,   275,   275,   275,   275,   275,   275,   275,   275,   275,
    -137,  -137,   275,   127,  -137,   132,    90,    58,   134,    91,
    -137,  -137,   140,  -137,  -137,    53,   275,  -137,    59,    85,
      68,    68,   100,   100,   100,   100,    86,    86,    82,    82,
      82,  -137,   236,  -137,  -137,   141,   142,   143,   145,   146,
    -137,   164,  -137,  -137,    31,  -137,   149,   150,   247,   169,
    -137,  -137,  -137,  -137,   120,  -137,  -137,   275,   171,  -137,
     173,   275,   275,    15,  -137,  -137,  -137,  -137,  -137,   275,
    -137,   176,   264,  -137,    91,  -137,  -137,  -137,   174,   175,
     177,   178,  -137,   179,  -137,  -137,   182,  -137,   200,   200,
     275,  -137,  -137,   159,  -137,   183,  -137,   200,   275,  -137,
     184,  -137,   200,  -137
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    27,    28,     0,     2,     3,     0,     0,     1,     4,
       5,    14,     0,    18,    15,    18,    16,     0,     0,     0,
      20,    80,    93,     0,     0,     0,     0,     0,     0,     0,
      48,    51,    53,    56,    61,    64,    68,    69,    81,     0,
       0,    90,     0,    17,     0,     0,     0,     0,    92,     0,
      73,    81,    72,    74,    75,    76,    21,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,     6,    85,     0,     0,     0,     0,
      19,    23,     0,    70,    82,     0,     0,    79,    50,    52,
      54,    55,    57,    58,    59,    60,    63,    62,    65,    66,
      67,    49,     0,    34,     8,     0,     0,     0,     0,     0,
      32,     0,    10,    13,     0,    12,     0,    81,     0,    84,
       7,    91,    22,    25,     0,    94,    71,     0,     0,    30,
       0,     0,     0,    43,    39,    40,     9,    11,    33,     0,
      86,     0,     0,    24,     0,    83,    95,    29,     0,     0,
      41,     0,    42,     0,    87,    88,     0,    26,     0,     0,
      45,    31,    89,    35,    37,     0,    44,     0,    47,    36,
       0,    46,     0,    38
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -137,  -137,   202,   -34,  -137,    95,     8,    74,   196,  -137,
     -78,  -137,     7,  -136,  -137,  -137,  -137,   -17,  -137,   155,
     158,    87,    35,    84,   -11,  -137,  -137,   144,  -137,  -137,
      13,  -137
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     3,     4,   110,   111,   112,   113,     6,    14,    20,
      80,   124,   114,   115,   151,   165,   170,   116,    30,    31,
      32,    33,    34,    35,    36,    37,    85,    41,   119,    42,
      38,    48
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      29,   123,    43,    17,    11,    74,    49,     7,     5,    12,
       7,    10,    19,    50,    52,    53,    54,    55,    21,    22,
       1,     2,   163,   164,    13,    40,    23,    78,    81,    82,
      84,   169,    24,    25,    44,    15,   173,    51,    51,    51,
      51,    51,   120,    45,    46,    26,    76,    56,    47,    77,
      17,    57,    27,    28,    18,   101,    98,    99,   100,    19,
       1,     2,    81,     1,     2,   126,   157,    39,   127,   128,
      51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
      51,    51,    51,     8,    40,   130,   117,    58,     1,     2,
      61,    62,    63,    64,    21,    22,    92,    93,    94,    95,
      75,   141,    23,    73,    79,    67,    68,    69,    24,    25,
     145,    59,    60,    72,   148,   149,   152,    65,    66,    70,
      71,    26,   153,    86,   117,   156,    87,    81,    27,    28,
      21,    22,     1,     2,   143,   144,   102,   103,    23,   118,
      73,   104,   122,   166,    24,    25,    90,    91,   125,    96,
      97,   171,   131,   132,   133,   134,   135,    26,   105,   138,
     106,   107,   108,   109,    27,    28,   139,    21,    22,     1,
       2,   117,   117,   102,   103,    23,   142,    73,   136,   146,
     117,    24,    25,   147,   154,   117,   158,   159,   160,   161,
     162,   167,    12,   168,    26,   105,   172,   106,   107,   108,
     109,    27,    28,    21,    22,     9,   137,   150,    16,   102,
     103,    23,    88,    73,    21,    22,    89,    24,    25,     0,
       0,   121,    23,    83,     0,     0,     0,     0,    24,    25,
      26,   105,     0,   106,   107,   108,   109,    27,    28,    21,
      22,    26,     0,     0,     0,     0,   129,    23,    27,    28,
      21,    22,     0,    24,    25,   140,     0,     0,    23,     0,
       0,     0,     0,     0,    24,    25,    26,    21,    22,     0,
       0,     0,   155,    27,    28,    23,     0,    26,    21,    22,
       0,    24,    25,     0,    27,    28,    23,     0,     0,     0,
       0,     0,    24,    25,    26,     0,     0,     0,     0,     0,
       0,    27,    28,     0,     0,    26,     0,     0,     0,     0,
       0,     0,    27,    28
};

static const yytype_int16 yycheck[] =
{
      17,    79,    19,     7,    10,    39,    23,     0,     0,    15,
       3,     3,    16,    24,    25,    26,    27,    28,     3,     4,
       5,     6,   158,   159,     4,    18,    11,    44,    45,    46,
      47,   167,    17,    18,     7,     4,   172,    24,    25,    26,
      27,    28,    76,    16,     7,    30,    12,     8,    11,    15,
       7,    29,    37,    38,    11,    72,    67,    68,    69,    16,
       5,     6,    79,     5,     6,    12,   144,    12,    15,    86,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,     0,    77,   102,    73,    28,     5,     6,
      22,    23,    24,    25,     3,     4,    61,    62,    63,    64,
       4,   118,    11,    13,    13,    19,    20,    21,    17,    18,
     127,    26,    27,    16,   131,   132,   133,    17,    18,    37,
      38,    30,   139,     7,   111,   142,    12,   144,    37,    38,
       3,     4,     5,     6,    14,    15,     9,    10,    11,     7,
      13,    14,     8,   160,    17,    18,    59,    60,     8,    65,
      66,   168,    11,    11,    11,    10,    10,    30,    31,    10,
      33,    34,    35,    36,    37,    38,    16,     3,     4,     5,
       6,   158,   159,     9,    10,    11,     7,    13,    14,     8,
     167,    17,    18,    10,     8,   172,    12,    12,    10,    10,
       8,    32,    15,    10,    30,    31,    12,    33,    34,    35,
      36,    37,    38,     3,     4,     3,   111,   133,    12,     9,
      10,    11,    57,    13,     3,     4,    58,    17,    18,    -1,
      -1,    77,    11,    12,    -1,    -1,    -1,    -1,    17,    18,
      30,    31,    -1,    33,    34,    35,    36,    37,    38,     3,
       4,    30,    -1,    -1,    -1,    -1,    10,    11,    37,    38,
       3,     4,    -1,    17,    18,     8,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    17,    18,    30,     3,     4,    -1,
      -1,    -1,     8,    37,    38,    11,    -1,    30,     3,     4,
      -1,    17,    18,    -1,    37,    38,    11,    -1,    -1,    -1,
      -1,    -1,    17,    18,    30,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     5,     6,    41,    42,    46,    47,    52,     0,    42,
      46,    10,    15,     4,    48,     4,    48,     7,    11,    16,
      49,     3,     4,    11,    17,    18,    30,    37,    38,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    70,    12,
      52,    67,    69,    57,     7,    16,     7,    11,    71,    57,
      64,    70,    64,    64,    64,    64,     8,    29,    28,    26,
      27,    22,    23,    24,    25,    17,    18,    19,    20,    21,
      37,    38,    16,    13,    43,     4,    12,    15,    57,    13,
      50,    57,    57,    12,    57,    66,     7,    12,    59,    60,
      61,    61,    62,    62,    62,    62,    63,    63,    64,    64,
      64,    57,     9,    10,    14,    31,    33,    34,    35,    36,
      43,    44,    45,    46,    52,    53,    57,    70,     7,    68,
      43,    67,     8,    50,    51,     8,    12,    15,    57,    10,
      57,    11,    11,    11,    10,    10,    14,    45,    10,    16,
       8,    57,     7,    14,    15,    57,     8,    10,    57,    57,
      47,    54,    57,    57,     8,     8,    57,    50,    12,    12,
      10,    10,     8,    53,    53,    55,    57,    32,    10,    53,
      56,    57,    12,    53
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    40,    41,    41,    41,    41,    42,    42,    43,    43,
      44,    44,    45,    45,    46,    47,    47,    48,    48,    48,
      48,    49,    49,    50,    50,    51,    51,    52,    52,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    54,    54,    54,    55,    55,    56,    56,    57,    57,
      58,    58,    59,    59,    60,    60,    60,    61,    61,    61,
      61,    61,    62,    62,    62,    63,    63,    63,    63,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    65,
      65,    65,    66,    66,    67,    67,    68,    68,    68,    68,
      69,    69,    70,    70,    71,    71
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     5,     6,     2,     3,
       1,     2,     1,     1,     2,     2,     3,     3,     1,     4,
       2,     3,     4,     1,     3,     1,     3,     1,     1,     3,
       2,     4,     1,     2,     1,     5,     7,     5,     9,     2,
       2,     1,     1,     0,     1,     0,     1,     0,     1,     3,
       3,     1,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     3,     1,     1,
       3,     4,     2,     2,     2,     2,     2,     2,     2,     3,
       1,     1,     1,     3,     3,     2,     2,     3,     3,     4,
       1,     3,     2,     1,     3,     4
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


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
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


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




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
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
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

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
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
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

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* CompileUnit: FuncDef  */
#line 155 "frontend/flexbison/MiniC.y"
                      {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, (yyvsp[0].node));

		// 设置到全局变量中
		ast_root = (yyval.node);
	}
#line 1396 "MiniC.tab.c"
    break;

  case 3: /* CompileUnit: VarDecl  */
#line 163 "frontend/flexbison/MiniC.y"
                  {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, (yyvsp[0].node));
		ast_root = (yyval.node);
	}
#line 1407 "MiniC.tab.c"
    break;

  case 4: /* CompileUnit: CompileUnit FuncDef  */
#line 169 "frontend/flexbison/MiniC.y"
                              {

		// 把函数定义的节点作为编译单元的孩子
		(yyval.node) = (yyvsp[-1].node)->insert_son_node((yyvsp[0].node));
	}
#line 1417 "MiniC.tab.c"
    break;

  case 5: /* CompileUnit: CompileUnit VarDecl  */
#line 174 "frontend/flexbison/MiniC.y"
                              {
		// 把变量定义的节点作为编译单元的孩子
		(yyval.node) = (yyvsp[-1].node)->insert_son_node((yyvsp[0].node));
	}
#line 1426 "MiniC.tab.c"
    break;

  case 6: /* FuncDef: BasicType T_ID T_L_PAREN T_R_PAREN Block  */
#line 181 "frontend/flexbison/MiniC.y"
                                                    {

		// 函数返回类型
		type_attr funcReturnType = (yyvsp[-4].type);

		// 函数名
		var_id_attr funcId = (yyvsp[-3].var_id);

		// 函数体节点即Block，即$5
		ast_node * blockNode = (yyvsp[0].node);

		// 形参结点没有，设置为空指针
		ast_node * formalParamsNode = nullptr;

		// 创建函数定义的节点，孩子有类型，函数名，语句块和形参(实际上无)
		// create_func_def函数内会释放funcId中指向的标识符空间，切记，之后不要再释放，之前一定要是通过strdup函数或者malloc分配的空间
		(yyval.node) = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
#line 1449 "MiniC.tab.c"
    break;

  case 7: /* FuncDef: BasicType T_ID T_L_PAREN FormalParamList T_R_PAREN Block  */
#line 199 "frontend/flexbison/MiniC.y"
                                                                    {

		// 函数返回类型
		type_attr funcReturnType = (yyvsp[-5].type);

		// 函数名
		var_id_attr funcId = (yyvsp[-4].var_id);

		// 形参列表节点
		ast_node * formalParamsNode = (yyvsp[-2].node);

		// 函数体节点即Block，即$6
		ast_node * blockNode = (yyvsp[0].node);

		// 创建函数定义的节点，孩子有类型，函数名，语句块和形参
		(yyval.node) = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
#line 1471 "MiniC.tab.c"
    break;

  case 8: /* Block: T_L_BRACE T_R_BRACE  */
#line 221 "frontend/flexbison/MiniC.y"
                            {
		// 语句块没有语句

		// 为了方便创建一个空的Block节点
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_BLOCK);
	}
#line 1482 "MiniC.tab.c"
    break;

  case 9: /* Block: T_L_BRACE BlockItemList T_R_BRACE  */
#line 227 "frontend/flexbison/MiniC.y"
                                            {
		// 语句块含有语句

		// BlockItemList归约时内部创建Block节点，并把语句加入，这里不创建Block节点
		(yyval.node) = (yyvsp[-1].node);
	}
#line 1493 "MiniC.tab.c"
    break;

  case 10: /* BlockItemList: BlockItem  */
#line 238 "frontend/flexbison/MiniC.y"
                          {
		// 第一个左侧的孩子节点归约成Block节点，后续语句可持续作为孩子追加到Block节点中
		// 创建一个AST_OP_BLOCK类型的中间节点，孩子为Statement($1)
		if ((yyvsp[0].node)) (yyval.node) = create_contain_node(ast_operator_type::AST_OP_BLOCK, (yyvsp[0].node));
		else (yyval.node) = create_contain_node(ast_operator_type::AST_OP_BLOCK);
	}
#line 1504 "MiniC.tab.c"
    break;

  case 11: /* BlockItemList: BlockItemList BlockItem  */
#line 244 "frontend/flexbison/MiniC.y"
                                  {
		// 把BlockItem归约的节点加入到BlockItemList的节点中
		if ((yyvsp[0].node)) (yyval.node) = (yyvsp[-1].node)->insert_son_node((yyvsp[0].node));
		else (yyval.node) = (yyvsp[-1].node);
	}
#line 1514 "MiniC.tab.c"
    break;

  case 12: /* BlockItem: Statement  */
#line 254 "frontend/flexbison/MiniC.y"
                       {
		// 语句节点传递给归约后的节点上，综合属性
		(yyval.node) = (yyvsp[0].node);
	}
#line 1523 "MiniC.tab.c"
    break;

  case 13: /* BlockItem: VarDecl  */
#line 258 "frontend/flexbison/MiniC.y"
                  {
		// 变量声明节点传递给归约后的节点上，综合属性
		(yyval.node) = (yyvsp[0].node);
	}
#line 1532 "MiniC.tab.c"
    break;

  case 14: /* VarDecl: VarDeclExpr T_SEMICOLON  */
#line 269 "frontend/flexbison/MiniC.y"
                                  {
		(yyval.node) = (yyvsp[-1].node);
	}
#line 1540 "MiniC.tab.c"
    break;

  case 15: /* VarDeclExpr: BasicType VarDef  */
#line 275 "frontend/flexbison/MiniC.y"
                              {

		// 创建类型节点
		ast_node * type_node = create_type_node((yyvsp[-1].type));

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, (yyvsp[0].node));
		decl_node->type = type_node->type;

		// 创建变量声明语句，并加入第一个变量
		(yyval.node) = create_var_decl_stmt_node(decl_node);
	}
#line 1557 "MiniC.tab.c"
    break;

  case 16: /* VarDeclExpr: VarDeclExpr T_COMMA VarDef  */
#line 287 "frontend/flexbison/MiniC.y"
                                     {

		// 创建类型节点，这里从VarDeclExpr获取类型，前面已经设置
		ast_node * type_node = ast_node::New((yyvsp[-2].node)->type);

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, (yyvsp[0].node));

		// 插入到变量声明语句
		(yyval.node) = (yyvsp[-2].node)->insert_son_node(decl_node);
	}
#line 1573 "MiniC.tab.c"
    break;

  case 17: /* VarDef: T_ID T_ASSIGN Expr  */
#line 301 "frontend/flexbison/MiniC.y"
                            {
		// 变量ID + 初始化
		ast_node *id_node = ast_node::New(var_id_attr{(yyvsp[-2].var_id).id, (yyvsp[-2].var_id).lineno});

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free((yyvsp[-2].var_id).id);

		// 创建一个初始化节点，孩子为变量名和初值表达式
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_VAR_INIT, id_node, (yyvsp[0].node));
	}
#line 1588 "MiniC.tab.c"
    break;

  case 18: /* VarDef: T_ID  */
#line 311 "frontend/flexbison/MiniC.y"
               {
		// 变量ID
		(yyval.node) = ast_node::New(var_id_attr{(yyvsp[0].var_id).id, (yyvsp[0].var_id).lineno});

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free((yyvsp[0].var_id).id);
	}
#line 1600 "MiniC.tab.c"
    break;

  case 19: /* VarDef: T_ID ArrayDims T_ASSIGN InitVal  */
#line 319 "frontend/flexbison/MiniC.y"
                                      {
        // 数组定义并初始化
        ast_node *id_node = ast_node::New(var_id_attr{(yyvsp[-3].var_id).id, (yyvsp[-3].var_id).lineno});
        free((yyvsp[-3].var_id).id);
        (yyval.node) = create_array_init_node(id_node, (yyvsp[-2].array_dims), (yyvsp[0].node));
    }
#line 1611 "MiniC.tab.c"
    break;

  case 20: /* VarDef: T_ID ArrayDims  */
#line 325 "frontend/flexbison/MiniC.y"
                     {
        // 数组定义（无初值）
        ast_node *id_node = ast_node::New(var_id_attr{(yyvsp[-1].var_id).id, (yyvsp[-1].var_id).lineno});
        free((yyvsp[-1].var_id).id);
        (yyval.node) = create_array_def_node(id_node, (yyvsp[0].array_dims));
    }
#line 1622 "MiniC.tab.c"
    break;

  case 21: /* ArrayDims: T_L_BRACKET Expr T_R_BRACKET  */
#line 336 "frontend/flexbison/MiniC.y"
                                         {
        // 一维
        (yyval.array_dims) = insert_array_dim(nullptr, (yyvsp[-1].node));
    }
#line 1631 "MiniC.tab.c"
    break;

  case 22: /* ArrayDims: ArrayDims T_L_BRACKET Expr T_R_BRACKET  */
#line 340 "frontend/flexbison/MiniC.y"
                                             {
        // 多维
        (yyval.array_dims) = insert_array_dim((yyvsp[-3].array_dims), (yyvsp[-1].node));
    }
#line 1640 "MiniC.tab.c"
    break;

  case 23: /* InitVal: Expr  */
#line 347 "frontend/flexbison/MiniC.y"
               {
		(yyval.node) = (yyvsp[0].node);
	}
#line 1648 "MiniC.tab.c"
    break;

  case 24: /* InitVal: T_L_BRACE InitValList T_R_BRACE  */
#line 350 "frontend/flexbison/MiniC.y"
                                      {
        // 初始化为列表
        (yyval.node) = create_array_init_list_node((yyvsp[-1].node));
    }
#line 1657 "MiniC.tab.c"
    break;

  case 25: /* InitValList: InitVal  */
#line 357 "frontend/flexbison/MiniC.y"
                      {
		(yyval.node) = create_array_init_list_node((yyvsp[0].node));
	}
#line 1665 "MiniC.tab.c"
    break;

  case 26: /* InitValList: InitValList T_COMMA InitVal  */
#line 360 "frontend/flexbison/MiniC.y"
                                  {
		(yyval.node) = (yyvsp[-2].node)->insert_son_node((yyvsp[0].node));
	}
#line 1673 "MiniC.tab.c"
    break;

  case 27: /* BasicType: T_INT  */
#line 366 "frontend/flexbison/MiniC.y"
                 {
		(yyval.type) = (yyvsp[0].type);
	}
#line 1681 "MiniC.tab.c"
    break;

  case 28: /* BasicType: T_VOID  */
#line 369 "frontend/flexbison/MiniC.y"
                 {
		(yyval.type) = (yyvsp[0].type);
	}
#line 1689 "MiniC.tab.c"
    break;

  case 29: /* Statement: T_RETURN Expr T_SEMICOLON  */
#line 378 "frontend/flexbison/MiniC.y"
                                      {
		// 返回语句

		// 创建返回节点AST_OP_RETURN，其孩子为Expr，即$2
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_RETURN, (yyvsp[-1].node));
	}
#line 1700 "MiniC.tab.c"
    break;

  case 30: /* Statement: T_RETURN T_SEMICOLON  */
#line 384 "frontend/flexbison/MiniC.y"
                               {
		// void函数的返回语句
		// 创建返回节点AST_OP_RETURN，其孩子为nullptr
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_RETURN, nullptr);
	}
#line 1710 "MiniC.tab.c"
    break;

  case 31: /* Statement: LVal T_ASSIGN Expr T_SEMICOLON  */
#line 389 "frontend/flexbison/MiniC.y"
                                         {
		// 赋值语句

		// 创建一个AST_OP_ASSIGN类型的中间节点，孩子为LVal($1)和Expr($3)
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_ASSIGN, (yyvsp[-3].node), (yyvsp[-1].node));
	}
#line 1721 "MiniC.tab.c"
    break;

  case 32: /* Statement: Block  */
#line 395 "frontend/flexbison/MiniC.y"
                {
		// 语句块

		// 内部已创建block节点，直接传递给Statement
		(yyval.node) = (yyvsp[0].node);
	}
#line 1732 "MiniC.tab.c"
    break;

  case 33: /* Statement: Expr T_SEMICOLON  */
#line 401 "frontend/flexbison/MiniC.y"
                           {
		// 表达式语句

		// 内部已创建表达式，直接传递给Statement
		(yyval.node) = (yyvsp[-1].node);
	}
#line 1743 "MiniC.tab.c"
    break;

  case 34: /* Statement: T_SEMICOLON  */
#line 407 "frontend/flexbison/MiniC.y"
                      {
		// 空语句

		// 直接返回空指针，需要再把语句加入到语句块时要注意判断，空语句不要加入
		(yyval.node) = nullptr;
	}
#line 1754 "MiniC.tab.c"
    break;

  case 35: /* Statement: T_IF T_L_PAREN Expr T_R_PAREN Statement  */
#line 413 "frontend/flexbison/MiniC.y"
                                                                        {
		// if语句

		// 创建if节点，孩子为条件表达式和语句
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_IF, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 1765 "MiniC.tab.c"
    break;

  case 36: /* Statement: T_IF T_L_PAREN Expr T_R_PAREN Statement T_ELSE Statement  */
#line 419 "frontend/flexbison/MiniC.y"
                                                                   {
		// if-else语句

		// 创建if-else节点，孩子为条件表达式、if语句和else语句
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_IF_ELSE, (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 1776 "MiniC.tab.c"
    break;

  case 37: /* Statement: T_WHILE T_L_PAREN Expr T_R_PAREN Statement  */
#line 425 "frontend/flexbison/MiniC.y"
                                                     {
		// while语句

		// 创建while节点，孩子为条件表达式和语句
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_WHILE, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 1787 "MiniC.tab.c"
    break;

  case 38: /* Statement: T_FOR T_L_PAREN ForInitStmt T_SEMICOLON ForCondExpr T_SEMICOLON ForStepExpr T_R_PAREN Statement  */
#line 431 "frontend/flexbison/MiniC.y"
                                                                                                          {
		// for (init; cond; step) statement
		// 创建 AST_OP_FOR 节点，子节点依次为 init、cond、step、stmt
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_FOR, (yyvsp[-6].node), (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 1797 "MiniC.tab.c"
    break;

  case 39: /* Statement: T_BREAK T_SEMICOLON  */
#line 436 "frontend/flexbison/MiniC.y"
                              {
		// break语句

		// 创建break节点
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_BREAK);
	}
#line 1808 "MiniC.tab.c"
    break;

  case 40: /* Statement: T_CONTINUE T_SEMICOLON  */
#line 442 "frontend/flexbison/MiniC.y"
                                 {
		// continue语句

		// 创建continue节点
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_CONTINUE);
	}
#line 1819 "MiniC.tab.c"
    break;

  case 41: /* ForInitStmt: VarDeclExpr  */
#line 450 "frontend/flexbison/MiniC.y"
                          {
		// 变量定义（如 int i = 0）
		(yyval.node) = (yyvsp[0].node);
	}
#line 1828 "MiniC.tab.c"
    break;

  case 42: /* ForInitStmt: Expr  */
#line 454 "frontend/flexbison/MiniC.y"
           {
		// 变量赋值（如 i = 0）
		(yyval.node) = (yyvsp[0].node); }
#line 1836 "MiniC.tab.c"
    break;

  case 43: /* ForInitStmt: %empty  */
#line 457 "frontend/flexbison/MiniC.y"
      {
		// 允许为空
		(yyval.node) = nullptr;
	}
#line 1845 "MiniC.tab.c"
    break;

  case 44: /* ForCondExpr: Expr  */
#line 463 "frontend/flexbison/MiniC.y"
                   {
		// 关系表达式（i <= 10）
		(yyval.node) = (yyvsp[0].node);
	}
#line 1854 "MiniC.tab.c"
    break;

  case 45: /* ForCondExpr: %empty  */
#line 467 "frontend/flexbison/MiniC.y"
      {
		// 允许为空
		(yyval.node) = nullptr;
	}
#line 1863 "MiniC.tab.c"
    break;

  case 46: /* ForStepExpr: Expr  */
#line 473 "frontend/flexbison/MiniC.y"
                   {
		// 变量赋值（i = i + 1）
		(yyval.node) = (yyvsp[0].node);
	}
#line 1872 "MiniC.tab.c"
    break;

  case 47: /* ForStepExpr: %empty  */
#line 477 "frontend/flexbison/MiniC.y"
      {
		// 允许为空
		(yyval.node) = nullptr;
	}
#line 1881 "MiniC.tab.c"
    break;

  case 48: /* Expr: LOrExp  */
#line 485 "frontend/flexbison/MiniC.y"
              {
		// 直接传递给归约后的节点
		(yyval.node) = (yyvsp[0].node);
	}
#line 1890 "MiniC.tab.c"
    break;

  case 49: /* Expr: LVal T_ASSIGN Expr  */
#line 489 "frontend/flexbison/MiniC.y"
                             {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_ASSIGN, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 1898 "MiniC.tab.c"
    break;

  case 50: /* LOrExp: LOrExp T_OR_OR LAndExp  */
#line 496 "frontend/flexbison/MiniC.y"
                             {
        // 逻辑或
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_LOGIC_OR, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1907 "MiniC.tab.c"
    break;

  case 51: /* LOrExp: LAndExp  */
#line 500 "frontend/flexbison/MiniC.y"
              {
        (yyval.node) = (yyvsp[0].node);
    }
#line 1915 "MiniC.tab.c"
    break;

  case 52: /* LAndExp: LAndExp T_AND_AND EqExp  */
#line 507 "frontend/flexbison/MiniC.y"
                              {
        // 逻辑与
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_LOGIC_AND, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1924 "MiniC.tab.c"
    break;

  case 53: /* LAndExp: EqExp  */
#line 511 "frontend/flexbison/MiniC.y"
            {
        (yyval.node) = (yyvsp[0].node);
    }
#line 1932 "MiniC.tab.c"
    break;

  case 54: /* EqExp: EqExp T_EQUAL RelExp  */
#line 525 "frontend/flexbison/MiniC.y"
                           {
        // 相等
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_EQUAL, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1941 "MiniC.tab.c"
    break;

  case 55: /* EqExp: EqExp T_NOT_EQUAL RelExp  */
#line 529 "frontend/flexbison/MiniC.y"
                               {
        // 不等
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_NOT_EQUAL, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1950 "MiniC.tab.c"
    break;

  case 56: /* EqExp: RelExp  */
#line 533 "frontend/flexbison/MiniC.y"
             {
        (yyval.node) = (yyvsp[0].node);
    }
#line 1958 "MiniC.tab.c"
    break;

  case 57: /* RelExp: RelExp T_LESS AddExp  */
#line 540 "frontend/flexbison/MiniC.y"
                           {
        // 小于
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_LESS, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1967 "MiniC.tab.c"
    break;

  case 58: /* RelExp: RelExp T_GREATER AddExp  */
#line 544 "frontend/flexbison/MiniC.y"
                              {
        // 大于
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_GREATER, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1976 "MiniC.tab.c"
    break;

  case 59: /* RelExp: RelExp T_LESS_EQUAL AddExp  */
#line 548 "frontend/flexbison/MiniC.y"
                                 {
        // 小于等于
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_LESS_EQUAL, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1985 "MiniC.tab.c"
    break;

  case 60: /* RelExp: RelExp T_GREATER_EQUAL AddExp  */
#line 552 "frontend/flexbison/MiniC.y"
                                    {
        // 大于等于
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_GREATER_EQUAL, (yyvsp[-2].node), (yyvsp[0].node));
    }
#line 1994 "MiniC.tab.c"
    break;

  case 61: /* RelExp: AddExp  */
#line 556 "frontend/flexbison/MiniC.y"
             {
        (yyval.node) = (yyvsp[0].node);
    }
#line 2002 "MiniC.tab.c"
    break;

  case 62: /* AddExp: AddExp T_ADD MulExp  */
#line 565 "frontend/flexbison/MiniC.y"
                          {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_ADD, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2010 "MiniC.tab.c"
    break;

  case 63: /* AddExp: AddExp T_SUB MulExp  */
#line 568 "frontend/flexbison/MiniC.y"
                          {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_SUB, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2018 "MiniC.tab.c"
    break;

  case 64: /* AddExp: MulExp  */
#line 571 "frontend/flexbison/MiniC.y"
             {
	(yyval.node) = (yyvsp[0].node);
	}
#line 2026 "MiniC.tab.c"
    break;

  case 65: /* MulExp: MulExp T_MUL UnaryExp  */
#line 578 "frontend/flexbison/MiniC.y"
                            {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_MUL, (yyvsp[-2].node), (yyvsp[0].node)); 
	}
#line 2034 "MiniC.tab.c"
    break;

  case 66: /* MulExp: MulExp T_DIV UnaryExp  */
#line 581 "frontend/flexbison/MiniC.y"
                            {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_DIV, (yyvsp[-2].node), (yyvsp[0].node)); 
	}
#line 2042 "MiniC.tab.c"
    break;

  case 67: /* MulExp: MulExp T_MOD UnaryExp  */
#line 584 "frontend/flexbison/MiniC.y"
                            {
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_MOD, (yyvsp[-2].node), (yyvsp[0].node)); 
	}
#line 2050 "MiniC.tab.c"
    break;

  case 68: /* MulExp: UnaryExp  */
#line 587 "frontend/flexbison/MiniC.y"
               {
		(yyval.node) = (yyvsp[0].node);
	}
#line 2058 "MiniC.tab.c"
    break;

  case 69: /* UnaryExp: PrimaryExp  */
#line 597 "frontend/flexbison/MiniC.y"
                      {
		// 基本表达式

		// 传递到归约后的UnaryExp上
		(yyval.node) = (yyvsp[0].node);
	}
#line 2069 "MiniC.tab.c"
    break;

  case 70: /* UnaryExp: T_ID T_L_PAREN T_R_PAREN  */
#line 603 "frontend/flexbison/MiniC.y"
                                   {
		// 没有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string((yyvsp[-2].var_id).id), (yyvsp[-2].var_id).lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free((yyvsp[-2].var_id).id);

		// 实参列表
		ast_node * paramListNode = nullptr;

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参为空，但函数内部会创建实参列表节点，无孩子
		(yyval.node) = create_func_call(name_node, paramListNode);

	}
#line 2090 "MiniC.tab.c"
    break;

  case 71: /* UnaryExp: T_ID T_L_PAREN RealParamList T_R_PAREN  */
#line 619 "frontend/flexbison/MiniC.y"
                                                 {
		// 含有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string((yyvsp[-3].var_id).id), (yyvsp[-3].var_id).lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free((yyvsp[-3].var_id).id);

		// 实参列表
		ast_node * paramListNode = (yyvsp[-1].node);

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参不为空
		(yyval.node) = create_func_call(name_node, paramListNode);
	}
#line 2110 "MiniC.tab.c"
    break;

  case 72: /* UnaryExp: T_ADD UnaryExp  */
#line 634 "frontend/flexbison/MiniC.y"
                         {
		// 单目求正运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_POS, (yyvsp[0].node));
	}
#line 2119 "MiniC.tab.c"
    break;

  case 73: /* UnaryExp: T_SUB UnaryExp  */
#line 638 "frontend/flexbison/MiniC.y"
                         {
        // 单目求负运算
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_NEG, (yyvsp[0].node));
    }
#line 2128 "MiniC.tab.c"
    break;

  case 74: /* UnaryExp: T_NOT UnaryExp  */
#line 642 "frontend/flexbison/MiniC.y"
                         {
		// 单目逻辑非运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_LOGIC_NOT, (yyvsp[0].node));
	}
#line 2137 "MiniC.tab.c"
    break;

  case 75: /* UnaryExp: T_INC UnaryExp  */
#line 646 "frontend/flexbison/MiniC.y"
                         {
		// 前置自增运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_PRE_INC, (yyvsp[0].node));
	}
#line 2146 "MiniC.tab.c"
    break;

  case 76: /* UnaryExp: T_DEC UnaryExp  */
#line 650 "frontend/flexbison/MiniC.y"
                         {
		// 前置自减运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_PRE_DEC, (yyvsp[0].node));
	}
#line 2155 "MiniC.tab.c"
    break;

  case 77: /* UnaryExp: UnaryExp T_INC  */
#line 654 "frontend/flexbison/MiniC.y"
                         {
		// 后置自增运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_POST_INC, (yyvsp[-1].node));
	}
#line 2164 "MiniC.tab.c"
    break;

  case 78: /* UnaryExp: UnaryExp T_DEC  */
#line 658 "frontend/flexbison/MiniC.y"
                         {
		// 后置自减运算
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_POST_DEC, (yyvsp[-1].node));
	}
#line 2173 "MiniC.tab.c"
    break;

  case 79: /* PrimaryExp: T_L_PAREN Expr T_R_PAREN  */
#line 667 "frontend/flexbison/MiniC.y"
                                       {
		// 带有括号的表达式
		(yyval.node) = (yyvsp[-1].node);
	}
#line 2182 "MiniC.tab.c"
    break;

  case 80: /* PrimaryExp: T_DIGIT  */
#line 671 "frontend/flexbison/MiniC.y"
                  {
        // 无符号整型字面量

		// 创建一个无符号整型的终结符节点
		(yyval.node) = ast_node::New((yyvsp[0].integer_num));
	}
#line 2193 "MiniC.tab.c"
    break;

  case 81: /* PrimaryExp: LVal  */
#line 677 "frontend/flexbison/MiniC.y"
                {
		// 具有左值的表达式

		// 直接传递到归约后的非终结符号PrimaryExp
		(yyval.node) = (yyvsp[0].node);
	}
#line 2204 "MiniC.tab.c"
    break;

  case 82: /* RealParamList: Expr  */
#line 708 "frontend/flexbison/MiniC.y"
                     {
		// 创建实参列表节点，并把当前的Expr节点加入
		(yyval.node) = create_contain_node(ast_operator_type::AST_OP_FUNC_REAL_PARAMS, (yyvsp[0].node));
	}
#line 2213 "MiniC.tab.c"
    break;

  case 83: /* RealParamList: RealParamList T_COMMA Expr  */
#line 712 "frontend/flexbison/MiniC.y"
                                     {
		// 左递归增加实参表达式
		(yyval.node) = (yyvsp[-2].node)->insert_son_node((yyvsp[0].node));
	}
#line 2222 "MiniC.tab.c"
    break;

  case 84: /* FormalParam: BasicType T_ID ArrayParamDims  */
#line 718 "frontend/flexbison/MiniC.y"
                                            {
        // type_node为类型节点，id_node为变量名节点，$3为数组维度链表
        ast_node *type_node = create_type_node((yyvsp[-2].type));
        ast_node *id_node = ast_node::New((yyvsp[-1].var_id));
        (yyval.node) = create_func_param_node(type_node, id_node, (yyvsp[0].array_dims));
    }
#line 2233 "MiniC.tab.c"
    break;

  case 85: /* FormalParam: BasicType T_ID  */
#line 724 "frontend/flexbison/MiniC.y"
                         {
		// 普通变量形参
        // 创建类型节点
        ast_node * type_node = create_type_node((yyvsp[-1].type));

        // 创建形参节点，类型为AST_OP_FUNC_FORMAL_PARAM，孩子为类型和变量名
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, type_node, ast_node::New((yyvsp[0].var_id)));
    }
#line 2246 "MiniC.tab.c"
    break;

  case 86: /* ArrayParamDims: T_L_BRACKET T_R_BRACKET  */
#line 739 "frontend/flexbison/MiniC.y"
                                         {
		// 形参首维可省略长度，如 int a[][3]
        (yyval.array_dims) = insert_array_dim(nullptr, nullptr);
    }
#line 2255 "MiniC.tab.c"
    break;

  case 87: /* ArrayParamDims: T_L_BRACKET Expr T_R_BRACKET  */
#line 743 "frontend/flexbison/MiniC.y"
                                   {
        (yyval.array_dims) = insert_array_dim(nullptr, (yyvsp[-1].node));
    }
#line 2263 "MiniC.tab.c"
    break;

  case 88: /* ArrayParamDims: ArrayParamDims T_L_BRACKET T_R_BRACKET  */
#line 746 "frontend/flexbison/MiniC.y"
                                             {
        (yyval.array_dims) = insert_array_dim((yyvsp[-2].array_dims), nullptr);
    }
#line 2271 "MiniC.tab.c"
    break;

  case 89: /* ArrayParamDims: ArrayParamDims T_L_BRACKET Expr T_R_BRACKET  */
#line 749 "frontend/flexbison/MiniC.y"
                                                  {
        (yyval.array_dims) = insert_array_dim((yyvsp[-3].array_dims), (yyvsp[-1].node));
    }
#line 2279 "MiniC.tab.c"
    break;

  case 90: /* FormalParamList: FormalParam  */
#line 756 "frontend/flexbison/MiniC.y"
                              {
        // 单个形参，创建AST_OP_FUNC_FORMAL_PARAMS节点
        (yyval.node) = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS, (yyvsp[0].node));
    }
#line 2288 "MiniC.tab.c"
    break;

  case 91: /* FormalParamList: FormalParamList T_COMMA FormalParam  */
#line 760 "frontend/flexbison/MiniC.y"
                                          {
        // 多个形参，递归插入
        (yyval.node) = (yyvsp[-2].node)->insert_son_node((yyvsp[0].node));
    }
#line 2297 "MiniC.tab.c"
    break;

  case 92: /* LVal: T_ID ArrayIndices  */
#line 767 "frontend/flexbison/MiniC.y"
                         {
        // id_node为变量名节点，$2为下标链表
        ast_node *id_node = ast_node::New((yyvsp[-1].var_id));
        free((yyvsp[-1].var_id).id);
        (yyval.node) = create_array_access_node(id_node, (yyvsp[0].array_indices)); // 你需实现
    }
#line 2308 "MiniC.tab.c"
    break;

  case 93: /* LVal: T_ID  */
#line 773 "frontend/flexbison/MiniC.y"
               {
		// 普通变量名
		// 变量名终结符
		// 创建变量名终结符节点
		(yyval.node) = ast_node::New((yyvsp[0].var_id));

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free((yyvsp[0].var_id).id);
	}
#line 2322 "MiniC.tab.c"
    break;

  case 94: /* ArrayIndices: T_L_BRACKET Expr T_R_BRACKET  */
#line 786 "frontend/flexbison/MiniC.y"
                                            {
        // 一维
        (yyval.array_indices) = insert_array_index(nullptr, (yyvsp[-1].node));
    }
#line 2331 "MiniC.tab.c"
    break;

  case 95: /* ArrayIndices: ArrayIndices T_L_BRACKET Expr T_R_BRACKET  */
#line 790 "frontend/flexbison/MiniC.y"
                                                {
        // 多维
        (yyval.array_indices) = insert_array_index((yyvsp[-3].array_indices), (yyvsp[-1].node));
    }
#line 2340 "MiniC.tab.c"
    break;


#line 2344 "MiniC.tab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

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
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

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
                      yytoken, &yylval);
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
  ++yynerrs;

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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 796 "frontend/flexbison/MiniC.y"


// 语法识别错误要调用函数的定义
void yyerror(char * msg)
{
    printf("Line %d: %s\n", yylineno, msg);
}
