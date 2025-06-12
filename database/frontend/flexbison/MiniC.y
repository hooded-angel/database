%{
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

%}

// 联合体声明，用于后续终结符和非终结符号属性指定使用
%union {
    class ast_node * node;

    struct digit_int_attr integer_num;
    struct digit_real_attr float_num;
    struct var_id_attr var_id;
    struct type_attr type;
    int op_class;

	// 数组维度和下标链表类型
    struct array_dim_node * array_dims;
    struct array_index_node * array_indices;
};

// 文法的开始符号
%start  CompileUnit

// 指定文法的终结符号，<>可指定文法属性
// 对于单个字符的算符或者分隔符，在词法分析时可直返返回对应的ASCII码值，bison预留了255以内的值
// %token开始的符号称之为终结符，需要词法分析工具如flex识别后返回
// %type开始的符号称之为非终结符，需要通过文法产生式来定义
// %token或%type之后的<>括住的内容成为文法符号的属性，定义在前面的%union中的成员名字。
%token <integer_num> T_DIGIT
%token <var_id> T_ID

// 基本类型，整型和void类型
%token <type> T_INT
%token <type> T_VOID

// 数组相关终结符和非终结符声明
%token T_L_BRACKET T_R_BRACKET

// 数组相关非终结符类型声明
%type <array_dims> ArrayDims
%type <array_dims> ArrayParamDims
%type <array_indices> ArrayIndices
%type <node> InitVal
%type <node> InitValList

// 函数定义
%type <node> FuncDef
%type <node> RealParamList
%type <node> FormalParamList
%type <node> FormalParam

// 函数返回值类型
// %type <type> FuncType

// 基本类型，用于变量声明
%type <type> BasicType

// 关键或保留字 一词一类 不需要赋予语义属性
%token T_RETURN

// 分隔符 一词一类 不需要赋予语义属性
%token T_SEMICOLON T_L_PAREN T_R_PAREN T_L_BRACE T_R_BRACE
%token T_COMMA

// 基本运算符
%token T_ASSIGN T_SUB T_ADD
%token T_MUL T_DIV T_MOD
%token T_LESS T_GREATER

// 关系运算符
%token T_LESS_EQUAL T_GREATER_EQUAL
%token T_EQUAL T_NOT_EQUAL

// 逻辑运算符
%token T_AND_AND T_OR_OR T_NOT

// T_IF 和 T_ELSE 词法单元
%token T_IF T_ELSE

// T_WHILE 词法单元
%token T_WHILE

// T_FOR 词法单元
%token T_FOR

// T_BREAK 和 T_CONTINUE 词法单元
%token T_BREAK T_CONTINUE

// 自增自减词法单元
%token T_INC T_DEC

// 非终结符
// %type指定文法的非终结符号，<>可指定文法属性
%type <node> CompileUnit
%type <node> Block
%type <node> BlockItemList
%type <node> BlockItem
%type <node> Statement
%type <node> Expr
%type <node> LVal
%type <node> VarDecl VarDeclExpr VarDef

// 表达式相关非终结符声明（优先级从低到高）
// 逻辑或表达式
%type <node> LOrExp
// 逻辑与表达式
%type <node> LAndExp
// 相等性表达式
%type <node> EqExp
// 关系表达式
%type <node> RelExp
// 加减表达式
%type <node> AddExp
// 乘除模表达式
%type <node> MulExp
// 一元表达式
%type <node> UnaryExp
// 基本表达式
%type <node> PrimaryExp
// 常量表达式
// %type <node> ConstExp
// 不再需要，放置到单目运算符中，即UnaryExp
// %type <node> LogicNotExp  // LogicNotExp 类型声明

// 辅助for循环的循环初始、循环控制、循环步进
%type <node> ForInitStmt
%type <node> ForCondExpr
%type <node> ForStepExpr

// 为了解决 if-else 语句的悬挂 else 问题，消除 shift/reduce 冲突
%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

%%

// 编译单元可包含若干个函数与全局变量定义。要在语义分析时检查main函数存在
// compileUnit: (funcDef | varDecl)* EOF;
// bison不支持闭包运算，为便于追加修改成左递归方式
// compileUnit: funcDef | varDecl | compileUnit funcDef | compileUnit varDecl
CompileUnit : FuncDef {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		$$ = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, $1);

		// 设置到全局变量中
		ast_root = $$;
	}
	| VarDecl {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		$$ = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, $1);
		ast_root = $$;
	}
	| CompileUnit FuncDef {

		// 把函数定义的节点作为编译单元的孩子
		$$ = $1->insert_son_node($2);
	}
	| CompileUnit VarDecl {
		// 把变量定义的节点作为编译单元的孩子
		$$ = $1->insert_son_node($2);
	}
	;

// 函数定义，目前支持整数返回类型，不支持形参
FuncDef : BasicType T_ID T_L_PAREN T_R_PAREN Block  {

		// 函数返回类型
		type_attr funcReturnType = $1;

		// 函数名
		var_id_attr funcId = $2;

		// 函数体节点即Block，即$5
		ast_node * blockNode = $5;

		// 形参结点没有，设置为空指针
		ast_node * formalParamsNode = nullptr;

		// 创建函数定义的节点，孩子有类型，函数名，语句块和形参(实际上无)
		// create_func_def函数内会释放funcId中指向的标识符空间，切记，之后不要再释放，之前一定要是通过strdup函数或者malloc分配的空间
		$$ = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
	| BasicType T_ID T_L_PAREN FormalParamList T_R_PAREN Block  {

		// 函数返回类型
		type_attr funcReturnType = $1;

		// 函数名
		var_id_attr funcId = $2;

		// 形参列表节点
		ast_node * formalParamsNode = $4;

		// 函数体节点即Block，即$6
		ast_node * blockNode = $6;

		// 创建函数定义的节点，孩子有类型，函数名，语句块和形参
		$$ = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
	;

// 语句块的文法Block ： T_L_BRACE BlockItemList? T_R_BRACE
// 其中?代表可有可无，在bison中不支持，需要拆分成两个产生式
// Block ： T_L_BRACE T_R_BRACE | T_L_BRACE BlockItemList T_R_BRACE
Block : T_L_BRACE T_R_BRACE {
		// 语句块没有语句

		// 为了方便创建一个空的Block节点
		$$ = create_contain_node(ast_operator_type::AST_OP_BLOCK);
	}
	| T_L_BRACE BlockItemList T_R_BRACE {
		// 语句块含有语句

		// BlockItemList归约时内部创建Block节点，并把语句加入，这里不创建Block节点
		$$ = $2;
	}
	;

// 语句块内语句列表的文法：BlockItemList : BlockItem+
// Bison不支持正闭包，需修改成左递归形式，便于属性的传递与孩子节点的追加
// 左递归形式的文法为：BlockItemList : BlockItem | BlockItemList BlockItem
BlockItemList : BlockItem {
		// 第一个左侧的孩子节点归约成Block节点，后续语句可持续作为孩子追加到Block节点中
		// 创建一个AST_OP_BLOCK类型的中间节点，孩子为Statement($1)
		if ($1) $$ = create_contain_node(ast_operator_type::AST_OP_BLOCK, $1);
		else $$ = create_contain_node(ast_operator_type::AST_OP_BLOCK);
	}
	| BlockItemList BlockItem {
		// 把BlockItem归约的节点加入到BlockItemList的节点中
		if ($2) $$ = $1->insert_son_node($2);
		else $$ = $1;
	}
	;


// 语句块中子项的文法：BlockItem : Statement
// 目前只支持语句,后续可增加支持变量定义
BlockItem : Statement  {
		// 语句节点传递给归约后的节点上，综合属性
		$$ = $1;
	}
	| VarDecl {
		// 变量声明节点传递给归约后的节点上，综合属性
		$$ = $1;
	}
	;

// 变量声明语句
// 语法：varDecl: basicType varDef (T_COMMA varDef)* T_SEMICOLON
// 因Bison不支持闭包运算符，因此需要修改成左递归，修改后的文法为：
// VarDecl : VarDeclExpr T_SEMICOLON
// VarDeclExpr: BasicType VarDef | VarDeclExpr T_COMMA varDef
VarDecl : VarDeclExpr T_SEMICOLON {
		$$ = $1;
	}
	;

// 变量声明表达式，可支持逗号分隔定义多个
VarDeclExpr: BasicType VarDef {

		// 创建类型节点
		ast_node * type_node = create_type_node($1);

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, $2);
		decl_node->type = type_node->type;

		// 创建变量声明语句，并加入第一个变量
		$$ = create_var_decl_stmt_node(decl_node);
	}
	| VarDeclExpr T_COMMA VarDef {

		// 创建类型节点，这里从VarDeclExpr获取类型，前面已经设置
		ast_node * type_node = ast_node::New($1->type);

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, $3);

		// 插入到变量声明语句
		$$ = $1->insert_son_node(decl_node);
	}
	;

// 变量定义包含变量名，实际上还有初值，这里没有实现。
VarDef : T_ID T_ASSIGN Expr {
		// 变量ID + 初始化
		ast_node *id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);

		// 创建一个初始化节点，孩子为变量名和初值表达式
		$$ = create_contain_node(ast_operator_type::AST_OP_VAR_INIT, id_node, $3);
	}
	| T_ID {
		// 变量ID
		$$ = ast_node::New(var_id_attr{$1.id, $1.lineno});

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);
	}
	// 数组定义/初始化（至少一维）
    | T_ID ArrayDims T_ASSIGN InitVal {
        // 数组定义并初始化
        ast_node *id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        free($1.id);
        $$ = create_array_init_node(id_node, $2, $4);
    }
    | T_ID ArrayDims {
        // 数组定义（无初值）
        ast_node *id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        free($1.id);
        $$ = create_array_def_node(id_node, $2);
    }
	;

// 支持多维数组定义，如 int a[2][3];
// ArrayDims : 递归收集每一维的表达式
// ArrayDims 至少有一项，不可空，避免与普通变量定义冲突
ArrayDims : T_L_BRACKET Expr T_R_BRACKET {
        // 一维
        $$ = insert_array_dim(nullptr, $2);
    }
    | ArrayDims T_L_BRACKET Expr T_R_BRACKET {
        // 多维
        $$ = insert_array_dim($1, $3);
    }
	;

// 支持数组初始化，如 int a[3] = {1, 2, 3}; 或 int b[2][2] = {{1,2},{3,4}};
InitVal : Expr {
		$$ = $1;
	}
    | T_L_BRACE InitValList T_R_BRACE {
        // 初始化为列表
        $$ = create_array_init_list_node($2);
    }
	;

// 初始化值列表，递归收集每个元素
InitValList : InitVal {
		$$ = create_array_init_list_node($1);
	}
    | InitValList T_COMMA InitVal {
		$$ = $1->insert_son_node($3);
	}
	;

// 基本类型，目前只支持整型
BasicType: T_INT {
		$$ = $1;
	}
	| T_VOID {
		$$ = $1;
	}
	;

// 语句文法：statement:T_RETURN expr T_SEMICOLON | lVal T_ASSIGN expr T_SEMICOLON
// | block | expr? T_SEMICOLON
// 支持返回语句、赋值语句、语句块、表达式语句
// 其中表达式语句可支持空语句，由于bison不支持?，修改成两条
Statement : T_RETURN Expr T_SEMICOLON {
		// 返回语句

		// 创建返回节点AST_OP_RETURN，其孩子为Expr，即$2
		$$ = create_contain_node(ast_operator_type::AST_OP_RETURN, $2);
	}
	| T_RETURN T_SEMICOLON {
		// void函数的返回语句
		// 创建返回节点AST_OP_RETURN，其孩子为nullptr
		$$ = create_contain_node(ast_operator_type::AST_OP_RETURN, nullptr);
	}
	| LVal T_ASSIGN Expr T_SEMICOLON {
		// 赋值语句

		// 创建一个AST_OP_ASSIGN类型的中间节点，孩子为LVal($1)和Expr($3)
		$$ = create_contain_node(ast_operator_type::AST_OP_ASSIGN, $1, $3);
	}
	| Block {
		// 语句块

		// 内部已创建block节点，直接传递给Statement
		$$ = $1;
	}
	| Expr T_SEMICOLON {
		// 表达式语句

		// 内部已创建表达式，直接传递给Statement
		$$ = $1;
	}
	| T_SEMICOLON {
		// 空语句

		// 直接返回空指针，需要再把语句加入到语句块时要注意判断，空语句不要加入
		$$ = nullptr;
	}
	| T_IF T_L_PAREN Expr T_R_PAREN Statement %prec LOWER_THAN_ELSE {
		// if语句

		// 创建if节点，孩子为条件表达式和语句
		$$ = create_contain_node(ast_operator_type::AST_OP_IF, $3, $5);
	}
	| T_IF T_L_PAREN Expr T_R_PAREN Statement T_ELSE Statement {
		// if-else语句

		// 创建if-else节点，孩子为条件表达式、if语句和else语句
		$$ = create_contain_node(ast_operator_type::AST_OP_IF_ELSE, $3, $5, $7);
	}
	| T_WHILE T_L_PAREN Expr T_R_PAREN Statement {
		// while语句

		// 创建while节点，孩子为条件表达式和语句
		$$ = create_contain_node(ast_operator_type::AST_OP_WHILE, $3, $5);
	}
	| T_FOR T_L_PAREN ForInitStmt T_SEMICOLON ForCondExpr T_SEMICOLON ForStepExpr T_R_PAREN Statement {
		// for (init; cond; step) statement
		// 创建 AST_OP_FOR 节点，子节点依次为 init、cond、step、stmt
		ast_node *node = create_contain_node(ast_operator_type::AST_OP_FOR, $3, $5, $7, $9);
        node->for_init = $3;
        node->for_cond = $5;
        node->for_step = $7;
        node->for_body = $9;
        $$ = node;
	}
	| T_BREAK T_SEMICOLON {
		// break语句

		// 创建break节点
		$$ = create_contain_node(ast_operator_type::AST_OP_BREAK);
	}
	| T_CONTINUE T_SEMICOLON {
		// continue语句

		// 创建continue节点
		$$ = create_contain_node(ast_operator_type::AST_OP_CONTINUE);
	}
	;

ForInitStmt : VarDeclExpr {
		// 变量定义（如 int i = 0）
		$$ = $1;
	}
    | Expr {
		// 变量赋值（如 i = 0）
		$$ = $1; }
    | {
		// 允许为空
		$$ = nullptr;
	}
    ;

ForCondExpr : Expr {
		// 关系表达式（i <= 10）
		$$ = $1;
	}
    | {
		// 允许为空
		$$ = nullptr;
	}
    ;

ForStepExpr : Expr {
		// 变量赋值（i = i + 1）
		$$ = $1;
	}
    | {
		// 允许为空
		$$ = nullptr;
	}
    ;

// 表达式文法 expr : AddExp
// 表达式目前只支持加法与减法运算
Expr : LOrExp {
		// 直接传递给归约后的节点
		$$ = $1;
	}
	| LVal T_ASSIGN Expr {
		$$ = create_contain_node(ast_operator_type::AST_OP_ASSIGN, $1, $3);
	}
	;

// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
LOrExp
    : LOrExp T_OR_OR LAndExp {
        // 逻辑或
        $$ = create_contain_node(ast_operator_type::AST_OP_LOGIC_OR, $1, $3);
    }
    | LAndExp {
        $$ = $1;
    }
    ;

// 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
LAndExp
    : LAndExp T_AND_AND EqExp {
        // 逻辑与
        $$ = create_contain_node(ast_operator_type::AST_OP_LOGIC_AND, $1, $3);
    }
    | EqExp {
        $$ = $1;
    }
    ;

// LogicNotExp
//     : T_NOT LogicNotExp
//         { $$ = create_contain_node(ast_operator_type::AST_OP_LOGIC_NOT, $2); }
//     | RelationExp
//         { $$ = $1; }
//     ;

// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
EqExp
    : EqExp T_EQUAL RelExp {
        // 相等
        $$ = create_contain_node(ast_operator_type::AST_OP_EQUAL, $1, $3);
    }
    | EqExp T_NOT_EQUAL RelExp {
        // 不等
        $$ = create_contain_node(ast_operator_type::AST_OP_NOT_EQUAL, $1, $3);
    }
    | RelExp {
        $$ = $1;
    }
    ;

// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
RelExp
    : RelExp T_LESS AddExp {
        // 小于
        $$ = create_contain_node(ast_operator_type::AST_OP_LESS, $1, $3);
    }
    | RelExp T_GREATER AddExp {
        // 大于
        $$ = create_contain_node(ast_operator_type::AST_OP_GREATER, $1, $3);
    }
    | RelExp T_LESS_EQUAL AddExp {
        // 小于等于
        $$ = create_contain_node(ast_operator_type::AST_OP_LESS_EQUAL, $1, $3);
    }
    | RelExp T_GREATER_EQUAL AddExp {
        // 大于等于
        $$ = create_contain_node(ast_operator_type::AST_OP_GREATER_EQUAL, $1, $3);
    }
    | AddExp {
        $$ = $1;
    }
    ;

// 由于bison不支持用闭包表达，因此需要拆分成左递归的形式
// 改造后的左递归文法：
// 加减表达式 AddExp → AddExp '+' MulExp | AddExp '-' MulExp | MulExp
AddExp
    : AddExp T_ADD MulExp {
		$$ = create_contain_node(ast_operator_type::AST_OP_ADD, $1, $3);
	}
    | AddExp T_SUB MulExp {
		$$ = create_contain_node(ast_operator_type::AST_OP_SUB, $1, $3);
	}
    | MulExp {
	$$ = $1;
	}
    ;

// 乘除模表达式 MulExp → MulExp '*' UnaryExp | MulExp '/' UnaryExp | MulExp '%' UnaryExp | UnaryExp
MulExp
    : MulExp T_MUL UnaryExp {
		$$ = create_contain_node(ast_operator_type::AST_OP_MUL, $1, $3); 
	}
    | MulExp T_DIV UnaryExp {
		$$ = create_contain_node(ast_operator_type::AST_OP_DIV, $1, $3); 
	}
    | MulExp T_MOD UnaryExp {
		$$ = create_contain_node(ast_operator_type::AST_OP_MOD, $1, $3); 
	}
    | UnaryExp {
		$$ = $1;
	}
    ;

// 目前一元表达式可以为基本表达式、函数调用，其中函数调用的实参可有可无
// 其文法为：unaryExp: primaryExp | T_ID T_L_PAREN realParamList? T_R_PAREN
// 由于bison不支持？表达，因此变更后的文法为：
// unaryExp: primaryExp | T_ID T_L_PAREN T_R_PAREN | T_ID T_L_PAREN realParamList T_R_PAREN
// 一元表达式 UnaryExp → PrimaryExp | T_ID '(' ')' | T_ID '(' FuncRParams ')' | '+' UnaryExp | '-' UnaryExp | '!' UnaryExp
UnaryExp : PrimaryExp {
		// 基本表达式

		// 传递到归约后的UnaryExp上
		$$ = $1;
	}
	| T_ID T_L_PAREN T_R_PAREN {
		// 没有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string($1.id), $1.lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);

		// 实参列表
		ast_node * paramListNode = nullptr;

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参为空，但函数内部会创建实参列表节点，无孩子
		$$ = create_func_call(name_node, paramListNode);

	}
	| T_ID T_L_PAREN RealParamList T_R_PAREN {
		// 含有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string($1.id), $1.lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);

		// 实参列表
		ast_node * paramListNode = $3;

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参不为空
		$$ = create_func_call(name_node, paramListNode);
	}
	| T_ADD UnaryExp {
		// 单目求正运算
		$$ = create_contain_node(ast_operator_type::AST_OP_POS, $2);
	}
	| T_SUB UnaryExp {
        // 单目求负运算
        $$ = create_contain_node(ast_operator_type::AST_OP_NEG, $2);
    }
	| T_NOT UnaryExp {
		// 单目逻辑非运算
		$$ = create_contain_node(ast_operator_type::AST_OP_LOGIC_NOT, $2);
	}
	| T_INC UnaryExp {
		// 前置自增运算
		$$ = create_contain_node(ast_operator_type::AST_OP_PRE_INC, $2);
	}
	| T_DEC UnaryExp {
		// 前置自减运算
		$$ = create_contain_node(ast_operator_type::AST_OP_PRE_DEC, $2);
	}
	| UnaryExp T_INC {
		// 后置自增运算
		$$ = create_contain_node(ast_operator_type::AST_OP_POST_INC, $1);
	}
	| UnaryExp T_DEC {
		// 后置自减运算
		$$ = create_contain_node(ast_operator_type::AST_OP_POST_DEC, $1);
	}
	;

// 基本表达式支持无符号整型字面量、带括号的表达式、具有左值属性的表达式
// 其文法为：primaryExp: T_L_PAREN expr T_R_PAREN | T_DIGIT | lVal
// 基本表达式 PrimaryExp → '(' Expr ')' | T_DIGIT | LVal
PrimaryExp :  T_L_PAREN Expr T_R_PAREN {
		// 带有括号的表达式
		$$ = $2;
	}
	| T_DIGIT {
        // 无符号整型字面量

		// 创建一个无符号整型的终结符节点
		$$ = ast_node::New($1);
	}
	| LVal  {
		// 具有左值的表达式

		// 直接传递到归约后的非终结符号PrimaryExp
		$$ = $1;
	}
	;

// 常量表达式 ConstExp → AddExp
// ConstExp : AddExp {
// 		// 常量表达式

// 		// 直接传递到归约后的非终结符号ConstExp
// 		$$ = $1;
// 	}
// 	;

// 函数返回值类型
// 支持 int 和 void 两种返回类型
// FuncType : T_VOID {
// 		$$ = $1;
// 	}
// 	| T_INT {
// 		$$ = $1;
// 	}
//     ;

// 实参表达式支持逗号分隔的若干个表达式
// 其文法为：realParamList: expr (T_COMMA expr)*
// 由于Bison不支持闭包运算符表达，修改成左递归形式的文法
// 左递归文法为：RealParamList : Expr | 左递归文法为：RealParamList T_COMMA expr
RealParamList : Expr {
		// 创建实参列表节点，并把当前的Expr节点加入
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_REAL_PARAMS, $1);
	}
	| RealParamList T_COMMA Expr {
		// 左递归增加实参表达式
		$$ = $1->insert_son_node($3);
	}
	;

FormalParam : BasicType T_ID ArrayParamDims {
        // type_node为类型节点，id_node为变量名节点，$3为数组维度链表
        ast_node *type_node = create_type_node($1);
        ast_node *id_node = ast_node::New($2);
        $$ = create_func_param_node(type_node, id_node, $3);
    }
	| BasicType T_ID {
		// 普通变量形参
        // 创建类型节点
        ast_node * type_node = create_type_node($1);

        // 创建形参节点，类型为AST_OP_FUNC_FORMAL_PARAM，孩子为类型和变量名
        $$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, type_node, ast_node::New($2));
    }
    // 如需支持数组形参，可扩展如下
    // | BasicType T_ID T_L_BRACKET T_R_BRACKET { ... }
    ;

// 形参支持数组，如 int a[], int b[2][3]
// ArrayParamDims : 递归收集形参数组的每一维
// 形参数组维度（至少一维）
ArrayParamDims : T_L_BRACKET T_R_BRACKET {
		// 形参首维可省略长度，如 int a[][3]
        $$ = insert_array_dim(nullptr, nullptr);
    }
    | T_L_BRACKET Expr T_R_BRACKET {
        $$ = insert_array_dim(nullptr, $2);
    }
    | ArrayParamDims T_L_BRACKET T_R_BRACKET {
        $$ = insert_array_dim($1, nullptr);
    }
    | ArrayParamDims T_L_BRACKET Expr T_R_BRACKET {
        $$ = insert_array_dim($1, $3);
    }
	;

// 形参列表声明
// 形参列表：一个或多个形参，逗号分隔
FormalParamList : FormalParam {
        // 单个形参，创建AST_OP_FUNC_FORMAL_PARAMS节点
        $$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS, $1);
    }
    | FormalParamList T_COMMA FormalParam {
        // 多个形参，递归插入
        $$ = $1->insert_son_node($3);
    }
    ;

// 左值表达式，目前只支持变量名，实际上还有下标变量
LVal : T_ID ArrayIndices {
        // id_node为变量名节点，$2为下标链表
        ast_node *id_node = ast_node::New($1);
        free($1.id);
        $$ = create_array_access_node(id_node, $2); // 你需实现
    }
	| T_ID {
		// 普通变量名
		// 变量名终结符
		// 创建变量名终结符节点
		$$ = ast_node::New($1);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);
	}
	;

// 多维数组下标，递归收集每一维的下标表达式
// 数组下标（至少一维）
ArrayIndices : T_L_BRACKET Expr T_R_BRACKET {
        // 一维
        $$ = insert_array_index(nullptr, $2);
    }
    | ArrayIndices T_L_BRACKET Expr T_R_BRACKET {
        // 多维
        $$ = insert_array_index($1, $3);
    }
	;

%%

// 语法识别错误要调用函数的定义
void yyerror(char * msg)
{
    printf("Line %d: %s\n", yylineno, msg);
}
