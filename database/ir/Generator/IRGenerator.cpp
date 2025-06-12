///
/// @file IRGenerator.cpp
/// @brief AST遍历产生线性IR的源文件
/// @author zenglj (zenglj@live.com)
/// @version 1.1
/// @date 2024-11-23
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-09-29 <td>1.0     <td>zenglj  <td>新建
/// <tr><td>2024-11-23 <td>1.1     <td>zenglj  <td>表达式版增强
/// </table>
///
#include <cstdint>
#include <cstdio>
#include <unordered_map>
#include <vector>
#include <iostream>

#include "AST.h"
#include "Common.h"
#include "Function.h"
#include "IRCode.h"
#include "IRGenerator.h"
#include "Module.h"
#include "EntryInstruction.h"
#include "LabelInstruction.h"
#include "ExitInstruction.h"
#include "FuncCallInstruction.h"
#include "BinaryInstruction.h"
#include "MoveInstruction.h"
#include "GotoInstruction.h"
#include "ArrayType.h"
#include "MemWriteInstruction.h"
#include "MemReadInstruction.h"
#include "PointerType.h"

/// @brief 构造函数
/// @param _root AST的根
/// @param _module 符号表
IRGenerator::IRGenerator(ast_node * _root, Module * _module) : root(_root), module(_module)
{
    /* 叶子节点 */
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_LITERAL_UINT] = &IRGenerator::ir_leaf_node_uint;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_VAR_ID] = &IRGenerator::ir_leaf_node_var_id;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_TYPE] = &IRGenerator::ir_leaf_node_type;

    /* 表达式加减运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_SUB] = &IRGenerator::ir_sub;
    ast2ir_handlers[ast_operator_type::AST_OP_ADD] = &IRGenerator::ir_add;

    /* 表达式乘除模运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_MUL] = &IRGenerator::ir_mul;
    ast2ir_handlers[ast_operator_type::AST_OP_DIV] = &IRGenerator::ir_div;
    ast2ir_handlers[ast_operator_type::AST_OP_MOD] = &IRGenerator::ir_mod;

    /* 表达式取负运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_NEG] = &IRGenerator::ir_neg;

    /* 单目前置后置自增自减运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_PRE_INC] = &IRGenerator::ir_pre_inc;
    ast2ir_handlers[ast_operator_type::AST_OP_PRE_DEC] = &IRGenerator::ir_pre_dec;
    ast2ir_handlers[ast_operator_type::AST_OP_POST_INC] = &IRGenerator::ir_post_inc;
    ast2ir_handlers[ast_operator_type::AST_OP_POST_DEC] = &IRGenerator::ir_post_dec;

    /* 表达式关系运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_GREATER] = &IRGenerator::ir_greater;
    ast2ir_handlers[ast_operator_type::AST_OP_LESS] = &IRGenerator::ir_less;
    ast2ir_handlers[ast_operator_type::AST_OP_GREATER_EQUAL] = &IRGenerator::ir_greater_equal;
    ast2ir_handlers[ast_operator_type::AST_OP_LESS_EQUAL] = &IRGenerator::ir_less_equal;
    ast2ir_handlers[ast_operator_type::AST_OP_EQUAL] = &IRGenerator::ir_equal;
    ast2ir_handlers[ast_operator_type::AST_OP_NOT_EQUAL] = &IRGenerator::ir_not_equal;

    /* 表达式逻辑运算 */
    ast2ir_handlers[ast_operator_type::AST_OP_LOGIC_AND] = &IRGenerator::ir_logic_and;
    ast2ir_handlers[ast_operator_type::AST_OP_LOGIC_OR] = &IRGenerator::ir_logic_or;
    ast2ir_handlers[ast_operator_type::AST_OP_LOGIC_NOT] = &IRGenerator::ir_logic_not;

    /* 语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_ASSIGN] = &IRGenerator::ir_assign;
    ast2ir_handlers[ast_operator_type::AST_OP_RETURN] = &IRGenerator::ir_return;

    /* if和if-else语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_IF] = &IRGenerator::ir_if_statement;
    ast2ir_handlers[ast_operator_type::AST_OP_IF_ELSE] = &IRGenerator::ir_if_else_statement;

    /* while语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_WHILE] = &IRGenerator::ir_while_statement;

    /* for语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_FOR] = &IRGenerator::ir_for_statement;

    /* break和continue语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_BREAK] = &IRGenerator::ir_break;
    ast2ir_handlers[ast_operator_type::AST_OP_CONTINUE] = &IRGenerator::ir_continue;

    /* 函数调用 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_CALL] = &IRGenerator::ir_function_call;

    /* 函数定义 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_DEF] = &IRGenerator::ir_function_define;
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS] = &IRGenerator::ir_function_formal_params;
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_FORMAL_PARAM] = &IRGenerator::ir_function_formal_param;

    /* 变量定义语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_DECL_STMT] = &IRGenerator::ir_declare_statment;
    ast2ir_handlers[ast_operator_type::AST_OP_VAR_DECL] = &IRGenerator::ir_variable_declare;

    /* 变量定义时赋初值 */
    ast2ir_handlers[ast_operator_type::AST_OP_VAR_INIT] = &IRGenerator::ir_variable_init;

    /* 数组定义和初始化 */
    ast2ir_handlers[ast_operator_type::AST_OP_ARRAY_ACCESS] = &IRGenerator::ir_array_access;

    /* 语句块 */
    ast2ir_handlers[ast_operator_type::AST_OP_BLOCK] = &IRGenerator::ir_block;

    /* 编译单元 */
    ast2ir_handlers[ast_operator_type::AST_OP_COMPILE_UNIT] = &IRGenerator::ir_compile_unit;
}

/// @brief 遍历抽象语法树产生线性IR，保存到IRCode中
/// @param root 抽象语法树
/// @param IRCode 线性IR
/// @return true: 成功 false: 失败
bool IRGenerator::run()
{
    ast_node * node;

    // 从根节点进行遍历
    node = ir_visit_ast_node(root);

    return node != nullptr;
}

/// @brief 根据AST的节点运算符查找对应的翻译函数并执行翻译动作
/// @param node AST节点
/// @return 成功返回node节点，否则返回nullptr
ast_node * IRGenerator::ir_visit_ast_node(ast_node * node)
{
    // 空节点
    if (nullptr == node) {
        return nullptr;
    }

    bool result;

    std::unordered_map<ast_operator_type, ast2ir_handler_t>::const_iterator pIter;
    pIter = ast2ir_handlers.find(node->node_type);
    if (pIter == ast2ir_handlers.end()) {
        // 没有找到，则说明当前不支持
        result = (this->ir_default)(node);
    } else {
        result = (this->*(pIter->second))(node);
    }

    if (!result) {
        // 语义解析错误，则出错返回
        node = nullptr;
    }

    return node;
}

/// @brief 未知节点类型的节点处理
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_default(ast_node * node)
{
    // 未知的节点
    printf("Unkown node(%d)\n", (int) node->node_type);
    return true;
}

/// @brief 编译单元AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_compile_unit(ast_node * node)
{
    module->setCurrentFunction(nullptr);

    for (auto son: node->sons) {

        // 遍历编译单元，要么是函数定义，要么是语句
        ast_node * son_node = ir_visit_ast_node(son);
        if (!son_node) {
            // TODO 自行追加语义错误处理
            return false;
        }
    }

    return true;
}

/// @brief 函数定义AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_define(ast_node * node)
{
    bool result;

    // 创建一个函数，用于当前函数处理
    if (module->getCurrentFunction()) {
        // 函数中嵌套定义函数，这是不允许的，错误退出
        // TODO 自行追加语义错误处理
        return false;
    }

    // 函数定义的AST包含四个孩子
    // 第一个孩子：函数返回类型
    // 第二个孩子：函数名字
    // 第三个孩子：形参列表
    // 第四个孩子：函数体即block
    ast_node * type_node = node->sons[0];
    ast_node * name_node = node->sons[1];
    ast_node * param_node = node->sons[2];
    ast_node * block_node = node->sons[3];

    // 1. 收集形参信息
    std::vector<FormalParam *> params;
    if (param_node && !param_node->sons.empty()) {
        for (auto param: param_node->sons) {
            // param->sons[0]: 类型节点
            // param->sons[1]: 变量名节点
            Type * paramType = param->sons[0]->type;
            // 检查是否为数组形参
            if (param->array_dims) {
                // 获取原始各维长度
                std::vector<int> dims;
                array_dim_node * p = param->array_dims;
                while (p) {
                    ast_node * dim_expr = p->expr;
                    int dim = (int) dim_expr->integer_val;
                    dims.push_back(dim);
                    p = p->next;
                }
                // 首维设为0，表示指针
                if (!dims.empty())
                    dims[0] = 0;
                paramType = ArrayType::get(paramType, dims);
            }
            params.push_back(new FormalParam{paramType, param->sons[1]->name});
        }
    }

    // 2. 用完整参数信息注册函数
    Function * newFunc = module->newFunction(name_node->name, type_node->type, params);
    if (!newFunc) {
        // 新定义的函数已经存在，则失败返回。
        // TODO 自行追加语义错误处理
        return false;
    }

    // 当前函数设置有效，变更为当前的函数
    module->setCurrentFunction(newFunc);

    // 进入函数的作用域
    module->enterScope();

    // 获取函数的IR代码列表，用于后面追加指令用，注意这里用的是引用传值
    InterCode & irCode = newFunc->getInterCode();

    // 这里也可增加一个函数入口Label指令，便于后续基本块划分

    // 创建并加入Entry入口指令
    irCode.addInst(new EntryInstruction(newFunc));

    // 创建出口指令并不加入出口指令，等函数内的指令处理完毕后加入出口指令
    LabelInstruction * exitLabelInst = new LabelInstruction(newFunc);

    // 函数出口指令保存到函数信息中，因为在语义分析函数体时return语句需要跳转到函数尾部，需要这个label指令
    newFunc->setExitLabel(exitLabelInst);

    // 遍历形参，没有IR指令，不需要追加
    result = ir_function_formal_params(param_node);
    if (!result) {
        // 形参解析失败
        // TODO 自行追加语义错误处理
        return false;
    }
    node->blockInsts.addInst(param_node->blockInsts);

    // 新建一个Value，用于保存函数的返回值，如果没有返回值可不用申请
    LocalVariable * retValue = nullptr;
    if (!type_node->type->isVoidType()) {

        // 保存函数返回值变量到函数信息中，在return语句翻译时需要设置值到这个变量中
        retValue = static_cast<LocalVariable *>(module->newVarValue(type_node->type));
    }
    newFunc->setReturnValue(retValue);

    // 这里最好设置返回值变量的初值为0，以便在没有返回值时能够返回0
    // if (name_node->name == "main" && retValue) {
    //     irCode.addInst(new MoveInstruction(newFunc, retValue, module->newConstInt(0)));
    // }

    // 函数内已经进入作用域，内部不再需要做变量的作用域管理
    block_node->needScope = false;

    // 遍历block
    result = ir_block(block_node);
    if (!result) {
        // block解析失败
        // TODO 自行追加语义错误处理
        return false;
    }

    // IR指令追加到当前的节点中
    node->blockInsts.addInst(block_node->blockInsts);

    // 此时，所有指令都加入到当前函数中，也就是node->blockInsts

    // node节点的指令移动到函数的IR指令列表中
    irCode.addInst(node->blockInsts);

    // 添加函数出口Label指令，主要用于return语句跳转到这里进行函数的退出
    irCode.addInst(exitLabelInst);

    // 函数出口指令
    irCode.addInst(new ExitInstruction(newFunc, retValue));

    // 恢复成外部函数
    module->setCurrentFunction(nullptr);

    // 退出函数的作用域
    module->leaveScope();

    return true;
}

/// @brief 形式参数AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_formal_params(ast_node * node)
{
    // TODO 目前形参还不支持，直接返回true
    // 每个形参变量都创建对应的临时变量，用于表达实参转递的值
    // 而真实的形参则创建函数内的局部变量。
    // 然后产生赋值指令，用于把表达实参值的临时变量拷贝到形参局部变量上。
    // 请注意这些指令要放在Entry指令后面，因此处理的先后上要注意。
    // 空参数直接返回
    if (!node || node->sons.empty())
        return true;

    Function * func = module->getCurrentFunction();

    // 1. 为每个形参生成局部变量
    for (size_t i = 0; i < node->sons.size(); ++i) {
        // param->sons[0]: 类型节点
        // param->sons[1]: 变量名节点
        ast_node * param = node->sons[i];
        Type * paramType = param->sons[0]->type;
        // 检查是否为数组形参
        if (param->array_dims) {
            std::vector<int> dims;
            array_dim_node * p = param->array_dims;
            while (p) {
                ast_node * dim_expr = p->expr;
                int dim = (int) dim_expr->integer_val;
                dims.push_back(dim);
                p = p->next;
            }
            if (!dims.empty())
                dims[0] = 0;
            paramType = ArrayType::get(paramType, dims);
        }
        // 分配局部变量（类型为首维0的数组类型）
        LocalVariable * localVar = static_cast<LocalVariable *>(module->newVarValue(paramType, param->sons[1]->name));
        // 生成赋值指令：localVar = argVar
        MoveInstruction * movInst = new MoveInstruction(func, localVar, func->getParams()[i]);
        node->blockInsts.addInst(movInst);
    }
    return true;
}

/// @brief 单个形参AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_formal_param(ast_node * node)
{
    // 对于单个形参节点，这里一般不需要生成IR，只需递归处理其子节点（类型和变量名），以保证属性传递
    // 如果需要，可以在这里做类型检查或符号表登记
    return true;
}

/// @brief 函数调用AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_call(ast_node * node)
{
    std::vector<Value *> realParams;

    // 获取当前正在处理的函数
    Function * currentFunc = module->getCurrentFunction();

    // 函数调用的节点包含两个节点：
    // 第一个节点：函数名节点
    // 第二个节点：实参列表节点

    std::string funcName = node->sons[0]->name;
    int64_t lineno = node->sons[0]->line_no;

    ast_node * paramsNode = node->sons[1];

    // 根据函数名查找函数，看是否存在。若不存在则出错
    // 这里约定函数必须先定义后使用
    auto calledFunction = module->findFunction(funcName);
    if (nullptr == calledFunction) {
        minic_log(LOG_ERROR, "函数(%s)未定义或声明", funcName.c_str());
        return false;
    }

    // 当前函数存在函数调用
    currentFunc->setExistFuncCall(true);

    // 如果没有孩子，也认为是没有参数
    if (!paramsNode->sons.empty()) {

        int32_t argsCount = (int32_t) paramsNode->sons.size();

        // 当前函数中调用函数实参个数最大值统计，实际上是统计实参传参需在栈中分配的大小
        // 因为目前的语言支持的int和float都是四字节的，只统计个数即可
        if (argsCount > currentFunc->getMaxFuncCallArgCnt()) {
            currentFunc->setMaxFuncCallArgCnt(argsCount);
        }

        // 遍历参数列表，孩子是表达式
        // 这里自左往右计算表达式
        for (auto son: paramsNode->sons) {

            // 遍历Block的每个语句，进行显示或者运算
            ast_node * temp = ir_visit_ast_node(son);
            if (!temp) {
                return false;
            }

            if (temp->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
                // 判断是否下标给全
                Value * paramVal = temp->val;
                // 获取数组类型和下标数
                Value * base = module->findVarValue(temp->sons[0]->name);
                ArrayType * arrType = dynamic_cast<ArrayType *>(base->getType());
                int total_dims = arrType ? arrType->getDims().size() : 0;
                int used_dims = 0;
                array_index_node * p = temp->array_indices;
                while (p) {
                    used_dims++;
                    p = p->next;
                }

                node->blockInsts.addInst(temp->blockInsts);

                if (arrType && used_dims == total_dims) {
                    // 下标给全，生成load指令
                    MemReadInstruction * load_inst =
                        new MemReadInstruction(currentFunc, paramVal, arrType->getElementType());
                    node->blockInsts.addInst(load_inst);
                    paramVal = load_inst;
                }
                // 否则，未给全，直接传地址和降维类型
                realParams.push_back(paramVal);
            } else {
                node->blockInsts.addInst(temp->blockInsts);
                realParams.push_back(temp->val);
            }
            // Value * paramVal = temp->val;
            // // 如果是数组访问，取其右值（即生成load指令，取出元素值）
            // if (temp->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
            //     // 生成load指令
            //     MemReadInstruction * load_inst =
            //         new MemReadInstruction(currentFunc, paramVal, IntegerType::getTypeInt());
            //     node->blockInsts.addInst(temp->blockInsts); // 先加上地址计算相关IR
            //     node->blockInsts.addInst(load_inst);        // 再加load指令
            //     paramVal = load_inst;
            // } else {
            //     node->blockInsts.addInst(temp->blockInsts);
            // }

            // realParams.push_back(paramVal);
            // node->blockInsts.addInst(temp->blockInsts);
        }
    }

    // TODO 这里请追加函数调用的语义错误检查，这里只进行了函数参数的个数检查等，其它请自行追加。
    if (realParams.size() != calledFunction->getParams().size()) {
        // 函数参数的个数不一致，语义错误
        minic_log(LOG_ERROR, "第%lld行的被调用函数(%s)未定义或声明", (long long) lineno, funcName.c_str());
        return false;
    }

    // 返回调用有返回值，则需要分配临时变量，用于保存函数调用的返回值
    Type * type = calledFunction->getReturnType();

    FuncCallInstruction * funcCallInst = new FuncCallInstruction(currentFunc, calledFunction, realParams, type);

    // 创建函数调用指令
    node->blockInsts.addInst(funcCallInst);

    // 函数调用结果Value保存到node中，可能为空，上层节点可利用这个值
    node->val = funcCallInst;

    // 如果有真假出口，生成条件跳转
    if (node->trueLabel && node->falseLabel) {
        node->blockInsts.addInst(new GotoInstruction(currentFunc,
                                                     funcCallInst,
                                                     static_cast<LabelInstruction *>(node->trueLabel),
                                                     static_cast<LabelInstruction *>(node->falseLabel)));
    }

    return true;
}

/// @brief 语句块（含函数体）AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_block(ast_node * node)
{
    // 进入作用域
    if (node->needScope) {
        module->enterScope();
    }

    std::vector<ast_node *>::iterator pIter;
    for (pIter = node->sons.begin(); pIter != node->sons.end(); ++pIter) {

        // 遍历Block的每个语句，进行显示或者运算
        ast_node * temp = ir_visit_ast_node(*pIter);
        if (!temp) {
            return false;
        }

        node->blockInsts.addInst(temp->blockInsts);
    }

    // 离开作用域
    if (node->needScope) {
        module->leaveScope();
    }

    return true;
}

/// @brief 整数加法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_add(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * addInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_ADD_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(addInst);

    node->val = addInst;

    return true;
}

/// @brief 整数减法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_sub(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * subInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(subInst);

    node->val = subInst;

    return true;
}

/// @brief 整数乘法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_mul(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 乘法节点，左结合，先计算左节点，后计算右节点

    // 乘法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 乘法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * mulInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MUL_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(mulInst);

    node->val = mulInst;

    return true;
}

/// @brief 整数除法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_div(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 除法节点，左结合，先计算左节点，后计算右节点

    // 除法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        return false;
    }

    // 除法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * divInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_DIV_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(divInst);

    node->val = divInst;

    return true;
}

/// @brief 整数模运算AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_mod(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 模运算节点，左结合，先计算左节点，后计算右节点

    // 模运算的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        return false;
    }

    // 模运算的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * modInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MOD_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(modInst);

    node->val = modInst;

    return true;
}

/// @brief 整数求负AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
// bool IRGenerator::ir_neg(ast_node * node)
// {
//     ast_node * src_node = node->sons[0];
//     ast_node * operand = ir_visit_ast_node(src_node);
//     if (!operand)
//         return false;

//     Value * val = operand->val;

//     // 如果是i1类型（布尔），先转为int32
//     if (val->getType()->isInt1Byte()) {
//         // bool类型转换为int类型，真为1，假为0
//         Value * intVar = module->newVarValue(IntegerType::getTypeInt());
//         MoveInstruction * movTrue = new MoveInstruction(module->getCurrentFunction(), intVar,
//         module->newConstInt(1)); MoveInstruction * movFalse = new MoveInstruction(module->getCurrentFunction(),
//         intVar, module->newConstInt(0)); LabelInstruction * trueLabel = new
//         LabelInstruction(module->getCurrentFunction()); LabelInstruction * falseLabel = new
//         LabelInstruction(module->getCurrentFunction()); LabelInstruction * endLabel = new
//         LabelInstruction(module->getCurrentFunction()); node->blockInsts.addInst(operand->blockInsts);
//         node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(), val, trueLabel, falseLabel));
//         node->blockInsts.addInst(trueLabel);
//         node->blockInsts.addInst(movTrue);
//         node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(), endLabel));
//         node->blockInsts.addInst(falseLabel);
//         node->blockInsts.addInst(movFalse);
//         node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(), endLabel));
//         node->blockInsts.addInst(endLabel);

//         // 再对intVar求负
//         BinaryInstruction * negInst = new BinaryInstruction(module->getCurrentFunction(),
//                                                             IRInstOperator::IRINST_OP_NEG_I,
//                                                             intVar,
//                                                             nullptr,
//                                                             IntegerType::getTypeInt());
//         node->blockInsts.addInst(negInst);
//         node->val = negInst;
//         // 如果节点有真假出口label，则为其生成条件跳转IR指令
//         add_conditional_branch(node, negInst);
//         return true;
//     }

//     // 普通int类型
//     BinaryInstruction * negInst = new BinaryInstruction(module->getCurrentFunction(),
//                                                         IRInstOperator::IRINST_OP_NEG_I,
//                                                         val,
//                                                         nullptr,
//                                                         IntegerType::getTypeInt());
//     node->blockInsts.addInst(operand->blockInsts);
//     node->blockInsts.addInst(negInst);
//     node->val = negInst;

//     // 只有在有真假出口时才生成条件跳转
//     if (node->trueLabel && node->falseLabel) {
//         add_conditional_branch(node, negInst);
//     }

//     return true;
// }
bool IRGenerator::ir_neg(ast_node * node)
{
    ast_node * operand_node = node->sons[0];

    // 如果有真假出口，直接递归，不生成新的取负指令
    if (node->trueLabel && node->falseLabel) {
        operand_node->trueLabel = node->trueLabel;
        operand_node->falseLabel = node->falseLabel;
        ast_node * operand = ir_visit_ast_node(operand_node);
        if (!operand)
            return false;
        node->blockInsts.addInst(operand->blockInsts);
        return true;
    }

    // 普通表达式，生成取负指令
    ast_node * operand = ir_visit_ast_node(operand_node);
    if (!operand)
        return false;

    // 使用 getRValue 获取右值
    Value * val = getRValue(operand);
    BinaryInstruction * negInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_NEG_I,
                                                        val,
                                                        nullptr,
                                                        IntegerType::getTypeInt());
    node->blockInsts.addInst(operand->blockInsts);
    node->blockInsts.addInst(negInst);
    node->val = negInst;
    return true;
}

/// @brief 前置自增 ++i
bool IRGenerator::ir_pre_inc(ast_node * node)
{
    // 只支持变量或数组元素
    ast_node * lval_node = ir_visit_ast_node(node->sons[0]);
    if (!lval_node)
        return false;

    // 获取原值
    Value * oldVal = getRValue(lval_node);

    // 生成 newVal = oldVal + 1
    BinaryInstruction * addInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_ADD_I,
                                                        oldVal,
                                                        module->newConstInt(1),
                                                        IntegerType::getTypeInt());
    node->blockInsts.addInst(lval_node->blockInsts);
    node->blockInsts.addInst(addInst);

    // 写回
    if (lval_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        // *addr = newVal
        node->blockInsts.addInst(new MemWriteInstruction(module->getCurrentFunction(), lval_node->val, addInst));
    } else {
        // 普通变量
        node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), lval_node->val, addInst));
    }

    // 前置自增，表达式值为新值
    node->val = lval_node->val;
    return true;
}

/// @brief 前置自减 --i
bool IRGenerator::ir_pre_dec(ast_node * node)
{
    ast_node * lval_node = ir_visit_ast_node(node->sons[0]);
    if (!lval_node)
        return false;

    Value * oldVal = getRValue(lval_node);

    BinaryInstruction * subInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        oldVal,
                                                        module->newConstInt(1),
                                                        IntegerType::getTypeInt());
    node->blockInsts.addInst(lval_node->blockInsts);
    node->blockInsts.addInst(subInst);

    if (lval_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        node->blockInsts.addInst(new MemWriteInstruction(module->getCurrentFunction(), lval_node->val, subInst));
    } else {
        node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), lval_node->val, subInst));
    }

    node->val = lval_node->val;
    return true;
}

/// @brief 后置自增 i++
bool IRGenerator::ir_post_inc(ast_node * node)
{
    ast_node * lval_node = ir_visit_ast_node(node->sons[0]);
    if (!lval_node)
        return false;

    Value * oldVal = getRValue(lval_node);

    // 1. 先保存旧值到一个新的临时变量
    Value * tmpOld = module->newVarValue(oldVal->getType());
    node->blockInsts.addInst(lval_node->blockInsts);
    node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), tmpOld, oldVal));

    // 2. 计算新值
    BinaryInstruction * addInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_ADD_I,
                                                        oldVal,
                                                        module->newConstInt(1),
                                                        IntegerType::getTypeInt());
    node->blockInsts.addInst(addInst);

    // 3. 写回
    if (lval_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        node->blockInsts.addInst(new MemWriteInstruction(module->getCurrentFunction(), lval_node->val, addInst));
    } else {
        node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), lval_node->val, addInst));
    }

    // 4. 返回旧值
    node->val = tmpOld;
    return true;
}

/// @brief 后置自减 i--
bool IRGenerator::ir_post_dec(ast_node * node)
{
    ast_node * lval_node = ir_visit_ast_node(node->sons[0]);
    if (!lval_node)
        return false;

    Value * oldVal = getRValue(lval_node);

    // 1. 先保存旧值到一个新的临时变量
    Value * tmpOld = module->newVarValue(oldVal->getType());
    node->blockInsts.addInst(lval_node->blockInsts);
    node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), tmpOld, oldVal));

    // 2. 计算新值
    BinaryInstruction * subInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        oldVal,
                                                        module->newConstInt(1),
                                                        IntegerType::getTypeInt());
    node->blockInsts.addInst(subInst);

    // 3. 写回
    if (lval_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        node->blockInsts.addInst(new MemWriteInstruction(module->getCurrentFunction(), lval_node->val, subInst));
    } else {
        node->blockInsts.addInst(new MoveInstruction(module->getCurrentFunction(), lval_node->val, subInst));
    }

    // 4. 返回旧值
    node->val = tmpOld;
    return true;
}

// 关系运算符处理函数部分

/// @brief 如果AST节点有真假出口label，则为其生成条件跳转IR指令
/// @param node 当前AST节点（通常为关系表达式节点）
/// @param cond 条件值（关系表达式的结果，类型为Value*）
///
/// 用法：在关系表达式IR生成后调用本函数，
/// 若node->trueLabel和node->falseLabel均不为空，则生成
///     bc cond, label trueLabel, label falseLabel
/// 这样的条件跳转指令，实现if/while等语句的条件分支。
void IRGenerator::add_conditional_branch(ast_node * node, Value * cond)
{
    if (!node->trueLabel || !node->falseLabel) {
        fprintf(stderr,
                "add_conditional_branch: trueLabel or falseLabel is nullptr! node_type=%d\n",
                (int) node->node_type);
        abort();
    }

    if (node->trueLabel && node->falseLabel) {
        node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(),
                                                     cond,
                                                     static_cast<LabelInstruction *>(node->trueLabel),
                                                     static_cast<LabelInstruction *>(node->falseLabel)));
    }
}

/// @brief 整数大于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_greater(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 大于节点，左结合，先计算左节点，后计算右节点

    // 大于的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 大于的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * greaterInst = new BinaryInstruction(module->getCurrentFunction(),
                                                            IRInstOperator::IRINST_OP_GREATER_I,
                                                            leftVal,
                                                            rightVal,
                                                            IntegerType::getTypeBool());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(greaterInst);

    node->val = greaterInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, greaterInst);

    return true;
}

/// @brief 整数小于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_less(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 小于节点，左结合，先计算左节点，后计算右节点

    // 小于的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 小于的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * lessInst = new BinaryInstruction(module->getCurrentFunction(),
                                                         IRInstOperator::IRINST_OP_LESS_I,
                                                         leftVal,
                                                         rightVal,
                                                         IntegerType::getTypeBool());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(lessInst);

    node->val = lessInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, lessInst);

    return true;
}

/// @brief 整数大于等于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_greater_equal(ast_node * node)
{
    ast_node * src1_node = node->sons[0]; // 左操作数节点
    ast_node * src2_node = node->sons[1]; // 右操作数节点

    // 遍历左操作数（确保其生成IR）
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false; // 左操作数生成失败

    // 遍历右操作数（确保其生成IR）
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false; // 右操作数生成失败

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    // 创建布尔类型的比较指令（结果为i1）
    BinaryInstruction * greaterEqualInst =
        new BinaryInstruction(module->getCurrentFunction(),           // 当前函数
                              IRInstOperator::IRINST_OP_GREATER_EQ_I, // 大于等于操作码（整数比较）
                              leftVal,                                // 左操作数值
                              rightVal,                               // 右操作数值
                              IntegerType::getTypeBool()              // 结果类型为布尔（i1）
        );

    // 收集所有子节点的IR指令，并添加当前比较指令
    node->blockInsts.addInst(left->blockInsts);  // 左操作数的IR指令
    node->blockInsts.addInst(right->blockInsts); // 右操作数的IR指令
    node->blockInsts.addInst(greaterEqualInst);  // 当前比较指令

    // 将比较结果（BinaryInstruction）保存到当前节点
    node->val = greaterEqualInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, greaterEqualInst);

    return true;
}

/// @brief 整数小于等于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_less_equal(ast_node * node)
{
    ast_node * src1_node = node->sons[0]; // 左操作数节点
    ast_node * src2_node = node->sons[1]; // 右操作数节点

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    BinaryInstruction * lessEqualInst =
        new BinaryInstruction(module->getCurrentFunction(),        // 当前函数
                              IRInstOperator::IRINST_OP_LESS_EQ_I, // 小于等于操作码（整数比较）
                              leftVal,                             // 左操作数值
                              rightVal,                            // 右操作数值
                              IntegerType::getTypeBool()           // 结果类型为布尔（i1）
        );

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(lessEqualInst);

    node->val = lessEqualInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, lessEqualInst);

    return true;
}

/// @brief 整数等于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_equal(ast_node * node)
{
    ast_node * src1_node = node->sons[0]; // 左操作数节点
    ast_node * src2_node = node->sons[1]; // 右操作数节点

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    BinaryInstruction * equalInst = new BinaryInstruction(module->getCurrentFunction(), // 当前函数
                                                          IRInstOperator::IRINST_OP_EQUAL_I, // 等于操作码（整数比较）
                                                          leftVal,                           // 左操作数值
                                                          rightVal,                          // 右操作数值
                                                          IntegerType::getTypeBool() // 结果类型为布尔（i1）
    );

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(equalInst);

    node->val = equalInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, equalInst);

    return true;
}

/// @brief 整数不等于比较AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_not_equal(ast_node * node)
{
    ast_node * src1_node = node->sons[0]; // 左操作数节点
    ast_node * src2_node = node->sons[1]; // 右操作数节点

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    // 使用 getRValue 获取右值
    Value * leftVal = getRValue(left);
    Value * rightVal = getRValue(right);

    BinaryInstruction * notEqualInst =
        new BinaryInstruction(module->getCurrentFunction(),       // 当前函数
                              IRInstOperator::IRINST_OP_NOT_EQ_I, // 不等于操作码（整数比较）
                              leftVal,                            // 左操作数值
                              rightVal,                           // 右操作数值
                              IntegerType::getTypeBool()          // 结果类型为布尔（i1）
        );

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(notEqualInst);

    node->val = notEqualInst;

    // 如果节点有真假出口label，则为其生成条件跳转IR指令
    add_conditional_branch(node, notEqualInst);

    return true;
}

// 逻辑运算符处理函数部分

/// @brief 整数逻辑与AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_logic_and(ast_node * node)
{
    Function * func = module->getCurrentFunction();
    // 继承属性
    LabelInstruction * trueLabel = static_cast<LabelInstruction *>(node->trueLabel);
    LabelInstruction * falseLabel = static_cast<LabelInstruction *>(node->falseLabel);

    // 为左孩子分配一个新的label，作为左孩子为真时的跳转
    LabelInstruction * leftTrueLabel = new LabelInstruction(func);

    // 设置左孩子的出口
    node->sons[0]->trueLabel = leftTrueLabel;
    node->sons[0]->falseLabel = falseLabel;

    // 生成左孩子IR
    ir_visit_ast_node(node->sons[0]);
    node->blockInsts.addInst(node->sons[0]->blockInsts);

    // 在leftTrueLabel处生成右孩子
    node->blockInsts.addInst(leftTrueLabel);
    node->sons[1]->trueLabel = trueLabel;
    node->sons[1]->falseLabel = falseLabel;
    ir_visit_ast_node(node->sons[1]);
    node->blockInsts.addInst(node->sons[1]->blockInsts);

    return true;
}

/// @brief 整数逻辑或AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_logic_or(ast_node * node)
{
    Function * func = module->getCurrentFunction();
    LabelInstruction * trueLabel = static_cast<LabelInstruction *>(node->trueLabel);
    LabelInstruction * falseLabel = static_cast<LabelInstruction *>(node->falseLabel);

    // 为左孩子分配一个新的label，作为左孩子为假时的跳转
    LabelInstruction * leftFalseLabel = new LabelInstruction(func);

    node->sons[0]->trueLabel = trueLabel;
    node->sons[0]->falseLabel = leftFalseLabel;
    ir_visit_ast_node(node->sons[0]);
    node->blockInsts.addInst(node->sons[0]->blockInsts);

    node->blockInsts.addInst(leftFalseLabel);
    node->sons[1]->trueLabel = trueLabel;
    node->sons[1]->falseLabel = falseLabel;
    ir_visit_ast_node(node->sons[1]);
    node->blockInsts.addInst(node->sons[1]->blockInsts);

    return true;
}

/// @brief 整数逻辑非AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
// bool IRGenerator::ir_logic_not(ast_node * node)
// {
//     ast_node * operand_node = node->sons[0];
//     ast_node * operand = ir_visit_ast_node(operand_node);
//     if (!operand)
//         return false;

//     Value * val = operand->val;

//     // 如果节点有真假出口label，生成条件跳转
//     if (node->trueLabel && node->falseLabel) {
//         // 交换真假出口
//         operand_node->trueLabel = node->falseLabel;
//         operand_node->falseLabel = node->trueLabel;

//         // 递归生成子节点的IR
//         // ir_visit_ast_node(operand_node);
//         node->blockInsts.addInst(operand_node->blockInsts);
//         return true;
//     }

//     // 如果没有真假出口，生成一个==0的比较指令
//     if (val->getType()->isInt32Type()) {
//         BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
//                                                             IRInstOperator::IRINST_OP_EQUAL_I,
//                                                             val,
//                                                             module->newConstInt(0),
//                                                             IntegerType::getTypeBool());
//         node->blockInsts.addInst(operand->blockInsts);
//         node->blockInsts.addInst(cmpInst);
//         node->val = cmpInst;
//         return true;
//     }

//     // 其它情况直接复用原值
//     node->blockInsts.addInst(operand->blockInsts);
//     node->val = val;
//     return true;
// }
bool IRGenerator::ir_logic_not(ast_node * node)
{
    ast_node * operand_node = node->sons[0];

    // 交换真假出口
    operand_node->trueLabel = node->falseLabel;
    operand_node->falseLabel = node->trueLabel;

    // 只递归一次
    ast_node * operand = ir_visit_ast_node(operand_node);
    if (!operand)
        return false;

    // 只复用IR，不再递归
    node->blockInsts.addInst(operand->blockInsts);
    return true;
}

/// @brief 赋值AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_assign(ast_node * node)
{
    ast_node * son1_node = node->sons[0];
    ast_node * son2_node = node->sons[1];

    // 赋值节点，自右往左运算

    // 1. 处理赋值运算符左侧操作数
    ast_node * left = ir_visit_ast_node(son1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 2. 处理赋值运算符右侧操作数
    ast_node * right = ir_visit_ast_node(son2_node);
    if (!right) {
        return false;
    }

    // 3. 提取右值：如果是数组访问，则需要加载其值（*addr）
    Value * rightVal = right->val;
    if (right->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        // 添加右侧数组访问表达式生成的IR
        node->blockInsts.addInst(right->blockInsts);

        // 插入内存读指令：rightVal = *right->val
        MemReadInstruction * load_inst =
            new MemReadInstruction(module->getCurrentFunction(), right->val, IntegerType::getTypeInt());

        node->blockInsts.addInst(load_inst);
        rightVal = load_inst;
    } else {
        node->blockInsts.addInst(right->blockInsts);
    }

    // 4. 判断左值是否为数组元素
    if (left->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        // 添加左侧地址计算相关IR
        node->blockInsts.addInst(left->blockInsts);

        // 插入内存写指令：*left->val = rightVal
        MemWriteInstruction * memwrite_inst =
            new MemWriteInstruction(module->getCurrentFunction(), left->val, rightVal);
        node->blockInsts.addInst(memwrite_inst);
        node->val = memwrite_inst;
        return true;
    }

    // 5. 普通变量赋值（不涉及内存访问）
    node->blockInsts.addInst(left->blockInsts);
    MoveInstruction * movInst = new MoveInstruction(module->getCurrentFunction(), left->val, rightVal);
    node->blockInsts.addInst(movInst);

    // 返回最终结果
    node->val = movInst;
    return true;
}

/// @brief return节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_return(ast_node * node)
{
    ast_node * right = nullptr;

    // return语句可能没有没有表达式，也可能有，因此这里必须进行区分判断
    if (!node->sons.empty()) {

        ast_node * son_node = node->sons[0];

        // 返回的表达式的指令保存在right节点中
        right = ir_visit_ast_node(son_node);
        if (!right) {

            // 某个变量没有定值
            return false;
        }
    }

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理
    Function * currentFunc = module->getCurrentFunction();

    // 返回值存在时则移动指令到node中
    if (right) {

        // 创建临时变量保存IR的值，以及线性IR指令
        node->blockInsts.addInst(right->blockInsts);

        // 判断是否是数组访问，如果是则插入 MemReadInstruction
        Value * returnVal = right->val;
        if (right->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
            // 插入 *addr 的 load 指令，获取数组元素的值
            MemReadInstruction * load_inst = new MemReadInstruction(currentFunc, right->val, IntegerType::getTypeInt());
            node->blockInsts.addInst(load_inst);
            returnVal = load_inst;
        }

        // 返回值赋值到函数返回值变量上，然后跳转到函数的尾部
        node->blockInsts.addInst(new MoveInstruction(currentFunc, currentFunc->getReturnValue(), returnVal));

        node->val = returnVal;
    } else {
        // 没有返回值
        node->val = nullptr;
    }

    // 跳转到函数的尾部出口指令上
    node->blockInsts.addInst(new GotoInstruction(currentFunc, currentFunc->getExitLabel()));

    return true;
}

/// @brief 单分支 if(cond) stmt;
bool IRGenerator::ir_if_statement(ast_node * node)
{
    Function * func = module->getCurrentFunction();

    // 生成 then 和 end 的 label
    LabelInstruction * thenLabel = new LabelInstruction(func);
    LabelInstruction * endLabel = new LabelInstruction(func);

    // 设置真假出口
    ast_node * cond = node->sons[0];
    cond->trueLabel = thenLabel;
    cond->falseLabel = endLabel;

    // 生成条件表达式IR
    ir_visit_ast_node(cond);
    node->blockInsts.addInst(cond->blockInsts);

    // then 分支
    node->blockInsts.addInst(thenLabel);
    ast_node * thenStmt = ir_visit_ast_node(node->sons[1]);
    // if (!thenStmt)
    //     return false;
    // if语句块可能是空语句（;）
    if (thenStmt)
        node->blockInsts.addInst(thenStmt->blockInsts);

    // end
    node->blockInsts.addInst(endLabel);
    return true;
}

/// @brief 双分支 if(cond) stmt1; else stmt2;
bool IRGenerator::ir_if_else_statement(ast_node * node)
{
    Function * func = module->getCurrentFunction();

    // 三个 Label：then, else, end
    LabelInstruction * thenLabel = new LabelInstruction(func);
    LabelInstruction * elseLabel = new LabelInstruction(func);
    LabelInstruction * endLabel = new LabelInstruction(func);

    // 设置真假出口
    ast_node * cond = node->sons[0];
    cond->trueLabel = thenLabel;
    cond->falseLabel = elseLabel;

    // 生成条件表达式IR（短路风格）
    ir_visit_ast_node(cond);
    node->blockInsts.addInst(cond->blockInsts);

    // then 分支
    node->blockInsts.addInst(thenLabel);
    ast_node * thenStmt = ir_visit_ast_node(node->sons[1]);
    // if (!thenStmt)
    //     return false;
    // if语句块可能是空语句（;）
    if (thenStmt)
        node->blockInsts.addInst(thenStmt->blockInsts);
    // then 执行完跳到 end
    node->blockInsts.addInst(new GotoInstruction(func, endLabel));

    // else 分支
    node->blockInsts.addInst(elseLabel);
    ast_node * elseStmt = ir_visit_ast_node(node->sons[2]);
    // if (!elseStmt)
    //     return false;
    // else语句块可能是空语句（;）
    if (elseStmt)
        node->blockInsts.addInst(elseStmt->blockInsts);
    // else 执行完也跳到 end
    // node->blockInsts.addInst(new GotoInstruction(func, endLabel));

    // 结束
    node->blockInsts.addInst(endLabel);
    return true;
}

/// @brief while(cond) stmt;
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_while_statement(ast_node * node)
{
    Function * func = module->getCurrentFunction();

    // 创建循环头、体、尾的label
    LabelInstruction * condLabel = new LabelInstruction(func);
    LabelInstruction * bodyLabel = new LabelInstruction(func);
    LabelInstruction * endLabel = new LabelInstruction(func);

    // 进入循环，push label
    continueLabelStack.push_back(condLabel);
    breakLabelStack.push_back(endLabel);

    // 先跳到条件判断
    node->blockInsts.addInst(condLabel);

    // 设置真假出口
    ast_node * cond = node->sons[0];
    cond->trueLabel = bodyLabel;
    cond->falseLabel = endLabel;

    // 生成条件表达式IR
    ir_visit_ast_node(cond);
    node->blockInsts.addInst(cond->blockInsts);

    // 循环体
    node->blockInsts.addInst(bodyLabel);
    ast_node * bodyStmt = ir_visit_ast_node(node->sons[1]);
    // if (!bodyStmt)
    //     return false;
    // while循环体可能是空语句（;）
    if (bodyStmt)
        node->blockInsts.addInst(bodyStmt->blockInsts);

    // 循环体执行完跳回条件判断
    node->blockInsts.addInst(new GotoInstruction(func, condLabel));

    // 循环结束
    node->blockInsts.addInst(endLabel);

    // 离开循环，pop label
    continueLabelStack.pop_back();
    breakLabelStack.pop_back();

    return true;
}

bool IRGenerator::ir_for_statement(ast_node * node)
{
    Function * func = module->getCurrentFunction();

    // 进入for循环作用域
    module->enterScope();

    // for节点的子节点依次为：init, cond, step, body
    ast_node * init = node->for_init;
    ast_node * cond = node->for_cond;
    ast_node * step = node->for_step;
    ast_node * body = node->for_body;

    // 1. 生成初值表达式（可为空）
    if (init) {
        ast_node * initNode = ir_visit_ast_node(init);
        if (!initNode)
            return false;
        node->blockInsts.addInst(initNode->blockInsts);
    }

    // 2. 创建label
    LabelInstruction * condLabel = new LabelInstruction(func);
    LabelInstruction * bodyLabel = new LabelInstruction(func);
    LabelInstruction * stepLabel = new LabelInstruction(func);
    LabelInstruction * endLabel = new LabelInstruction(func);

    // 3. 初值后跳到条件判断
    node->blockInsts.addInst(new GotoInstruction(func, condLabel));

    // 4. 条件判断
    node->blockInsts.addInst(condLabel);
    if (cond) {
        cond->trueLabel = bodyLabel;
        cond->falseLabel = endLabel;
        ast_node * condNode = ir_visit_ast_node(cond);
        if (!condNode)
            return false;
        node->blockInsts.addInst(condNode->blockInsts);
    } else {
        // 条件为空，恒真，直接跳到body
        node->blockInsts.addInst(new GotoInstruction(func, bodyLabel));
    }

    // 5. 步进
    node->blockInsts.addInst(stepLabel);
    if (step) {
        ast_node * stepNode = ir_visit_ast_node(step);
        if (stepNode)
            node->blockInsts.addInst(stepNode->blockInsts);
    }

    // 6. 步进后跳回条件判断
    node->blockInsts.addInst(new GotoInstruction(func, condLabel));

    // 7. 循环体
    node->blockInsts.addInst(bodyLabel);

    // 支持break/continue嵌套
    continueLabelStack.push_back(stepLabel);
    breakLabelStack.push_back(endLabel);

    ast_node * bodyNode = ir_visit_ast_node(body);
    if (bodyNode)
        node->blockInsts.addInst(bodyNode->blockInsts);

    continueLabelStack.pop_back();
    breakLabelStack.pop_back();

    // 8. 循环体结束后跳到步进
    node->blockInsts.addInst(new GotoInstruction(func, stepLabel));

    // 9. 循环结束
    node->blockInsts.addInst(endLabel);

    // 离开for循环作用域
    module->leaveScope();

    return true;
}

/// @brief break;
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_break(ast_node * node)
{
    if (breakLabelStack.empty()) {
        // 语义错误：break不在循环内
        return false;
    }
    Function * func = module->getCurrentFunction();
    node->blockInsts.addInst(new GotoInstruction(func, breakLabelStack.back()));
    return true;
}

/// @brief continue;
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_continue(ast_node * node)
{
    if (continueLabelStack.empty()) {
        // 语义错误：continue不在循环内
        return false;
    }
    Function * func = module->getCurrentFunction();
    node->blockInsts.addInst(new GotoInstruction(func, continueLabelStack.back()));
    return true;
}

/// @brief 类型叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_type(ast_node * node)
{
    // 不需要做什么，直接从节点中获取即可。

    return true;
}

/// @brief 标识符叶子节点翻译成线性中间IR，变量声明的不走这个语句
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_var_id(ast_node * node)
{
    Value * val;

    // 查找ID型Value
    // 变量，则需要在符号表中查找对应的值

    val = module->findVarValue(node->name);

    node->val = val;

    // 如果有真假出口，生成条件跳转
    if (node->trueLabel && node->falseLabel) {
        node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(),
                                                     val,
                                                     static_cast<LabelInstruction *>(node->trueLabel),
                                                     static_cast<LabelInstruction *>(node->falseLabel)));
    }

    return true;
}

/// @brief 无符号整数字面量叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_uint(ast_node * node)
{
    ConstInt * val;

    // 新建一个整数常量Value
    val = module->newConstInt((int32_t) node->integer_val);

    node->val = val;

    // 如果有真假出口，直接根据常量值生成无条件跳转
    if (node->trueLabel && node->falseLabel) {
        if (val->getVal() == 0) {
            // 恒为假，直接跳到 falseLabel
            node->blockInsts.addInst(
                new GotoInstruction(module->getCurrentFunction(), static_cast<LabelInstruction *>(node->falseLabel)));
        } else {
            // 恒为真，直接跳到 trueLabel
            node->blockInsts.addInst(
                new GotoInstruction(module->getCurrentFunction(), static_cast<LabelInstruction *>(node->trueLabel)));
        }
    }
    // // 如果有真假出口，生成条件跳转
    // if (node->trueLabel && node->falseLabel) {
    //     node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(),
    //                                                  val,
    //                                                  static_cast<LabelInstruction *>(node->trueLabel),
    //                                                  static_cast<LabelInstruction *>(node->falseLabel)));
    // }

    return true;
}

/// @brief 变量声明语句节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_declare_statment(ast_node * node)
{
    bool result = false;

    for (auto & child: node->sons) {

        // 遍历每个变量声明
        result = ir_variable_declare(child);
        if (!result) {
            break;
        }
        // 合并每个声明的IR指令（包括初始化赋值）
        node->blockInsts.addInst(child->blockInsts);
    }

    return result;
}

/// @brief 变量声明节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_variable_declare(ast_node * node)
{
    // node->sons[0]: 类型节点
    // node->sons[1]: 变量名节点 或 var-init 节点
    ast_node * type_node = node->sons[0];
    ast_node * right = node->sons[1];

    // 检查是否为数组定义
    if (right->node_type == ast_operator_type::AST_OP_ARRAY_DEF ||
        right->node_type == ast_operator_type::AST_OP_ARRAY_INIT) {
        // 1. 获取每一维长度
        std::vector<int> dims;
        array_dim_node * p = right->array_dims;
        while (p) {
            // 假设维度表达式是常量
            ast_node * dim_expr = p->expr;
            int dim = (int) dim_expr->integer_val; // 需保证是常量
            dims.push_back(dim);
            p = p->next;
        }
        // 2. 构造数组类型
        Type * arrType = ArrayType::get(type_node->type, dims);
        // 3. 分配变量
        right->val = module->newVarValue(arrType, right->sons[0]->name);
        // 4. 其它初始化等
        return true;
    }

    if (right->node_type == ast_operator_type::AST_OP_VAR_INIT) {
        // 声明变量
        ast_node * var_node = right->sons[0]; // var-init的左孩子是变量名
        var_node->val = module->newVarValue(type_node->type, var_node->name);

        // 处理初始化赋值
        bool ok = ir_variable_init(right);
        // 合并初始化赋值的IR指令到当前声明节点
        node->blockInsts.addInst(right->blockInsts);
        return ok;
    } else {
        // 普通声明
        right->val = module->newVarValue(type_node->type, right->name);
        return true;
    }
}

/// @brief 变量定义时赋初值语句节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_variable_init(ast_node * node)
{
    // sons[0]: 变量名节点，sons[1]: 初值表达式
    ast_node * var_node = node->sons[0];
    ast_node * expr_node = node->sons[1];

    // 生成初值表达式
    ast_node * expr = ir_visit_ast_node(expr_node);
    if (!expr)
        return false;

    // 提取右值：如果是数组访问，需要从地址中读取值
    Value * exprVal = expr_node->val;
    if (expr_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        // 添加右侧 IR 指令
        node->blockInsts.addInst(expr_node->blockInsts);

        // 加载数组元素的值：exprVal = *expr_node->val
        MemReadInstruction * load_inst =
            new MemReadInstruction(module->getCurrentFunction(), expr_node->val, IntegerType::getTypeInt());
        node->blockInsts.addInst(load_inst);

        exprVal = load_inst;
    } else {
        // 普通右值
        node->blockInsts.addInst(expr_node->blockInsts);
    }

    // 生成赋值指令
    MoveInstruction * movInst = new MoveInstruction(module->getCurrentFunction(),
                                                    var_node->val, // 变量的Value（已在声明时设置）
                                                    exprVal        // 初值表达式的Value
    );

    // 判断是否为全局变量，用 dynamic_cast 判断
    auto * gvar = dynamic_cast<GlobalVariable *>(var_node->val);
    if (gvar) {
        // 是全局变量，初值必须是常量
        Value * initVal = exprVal;
        // 检查是否是常量
        if (auto * constInt = dynamic_cast<ConstInt *>(initVal)) {
            gvar->setInitValue(constInt);
        } else if (expr_node->node_type == ast_operator_type::AST_OP_NEG &&
                   expr_node->sons[0]->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
            // 特殊处理负数：直接生成负常量
            int val = (int) expr_node->sons[0]->integer_val;
            gvar->setInitValue(module->newConstInt(-val));
        } else {
            // 其它情况不支持，报错
            minic_log(LOG_ERROR, "全局变量初值必须是常量表达式，不能是运行期表达式");
            return false;
        }
        node->val = gvar;
        return true;
    }

    // 局部变量，正常生成赋值指令
    // 合并IR指令
    node->blockInsts.addInst(expr->blockInsts);
    node->blockInsts.addInst(movInst);

    node->val = var_node->val;
    return true;
}

/// @brief 数组访问AST节点翻译成线性中间IR
/// @param node AST节点（AST_OP_ARRAY_ACCESS）
/// @return 翻译是否成功
bool IRGenerator::ir_array_access(ast_node * node)
{
    // node->sons[0] 是数组名节点
    // node->array_indices 是下标链表

    // 1. 获取数组基址
    std::string arrayName = node->sons[0]->name;
    Value * base = module->findVarValue(arrayName);

    // 2. 获取数组类型和各维长度
    Type * type = base->getType();
    ArrayType * arrType = dynamic_cast<ArrayType *>(type);
    if (!arrType)
        return false;
    const std::vector<int> & dims = arrType->getDims();

    // 3. 递归生成每一维下标表达式IR
    std::vector<Value *> indices;
    array_index_node * p = node->array_indices;
    int idx_cnt = 0;
    while (p) {
        ast_node * idx_node = ir_visit_ast_node(p->expr);
        if (!idx_node)
            return false;
        // 修正：如果下标本身是数组访问，需要先取右值
        Value * idx_val = nullptr;
        if (idx_node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
            idx_val = getRValue(idx_node);
            // getRValue 里已经加了 MemReadInstruction 到 blockInsts
            node->blockInsts.addInst(idx_node->blockInsts);
        } else {
            idx_val = idx_node->val;
            node->blockInsts.addInst(idx_node->blockInsts);
        }
        indices.push_back(idx_val);
        p = p->next;
        idx_cnt++;
    }

    // 4. 生成降维公式 offset = (i1 * dim2 + i2) * dim3 + i3
    Value * offset = nullptr;
    for (int i = 0; i < idx_cnt; ++i) {
        int prod = 1;
        for (int j = i + 1; j < dims.size(); ++j)
            prod *= dims[j];
        Value * idx_val = indices[i];
        if (prod > 1) {
            BinaryInstruction * mul_inst = new BinaryInstruction(module->getCurrentFunction(),
                                                                 IRInstOperator::IRINST_OP_MUL_I,
                                                                 idx_val,
                                                                 module->newConstInt(prod),
                                                                 IntegerType::getTypeInt());
            node->blockInsts.addInst(mul_inst);
            idx_val = mul_inst;
        }
        if (!offset) {
            offset = idx_val;
        } else {
            BinaryInstruction * add_inst = new BinaryInstruction(module->getCurrentFunction(),
                                                                 IRInstOperator::IRINST_OP_ADD_I,
                                                                 offset,
                                                                 idx_val,
                                                                 IntegerType::getTypeInt());
            node->blockInsts.addInst(add_inst);
            offset = add_inst;
        }
    }
    // Value * offset = indices[0]; // 初始为第一维
    // for (int i = 1; i < idx_cnt; ++i) {
    //     // offset = offset * dim[i]
    //     BinaryInstruction * mul_inst = new BinaryInstruction(module->getCurrentFunction(),
    //                                                          IRInstOperator::IRINST_OP_MUL_I,
    //                                                          offset,
    //                                                          module->newConstInt(dims[i]),
    //                                                          IntegerType::getTypeInt());
    //     node->blockInsts.addInst(mul_inst);

    //     // offset = offset + indices[i]
    //     BinaryInstruction * add_inst = new BinaryInstruction(module->getCurrentFunction(),
    //                                                          IRInstOperator::IRINST_OP_ADD_I,
    //                                                          mul_inst,
    //                                                          indices[i],
    //                                                          IntegerType::getTypeInt());
    //     node->blockInsts.addInst(add_inst);

    //     offset = add_inst; // 更新offset
    // }

    // 5. 计算字节偏移 offset_bytes = offset * 4
    BinaryInstruction * offset_bytes_inst = nullptr;
    Value * offset_bytes = offset;
    if (offset) {
        offset_bytes_inst = new BinaryInstruction(module->getCurrentFunction(),
                                                  IRInstOperator::IRINST_OP_MUL_I,
                                                  offset,
                                                  module->newConstInt(4),
                                                  IntegerType::getTypeInt());
        node->blockInsts.addInst(offset_bytes_inst);
        offset_bytes = offset_bytes_inst;
    }

    // 6. 计算最终地址 addr = base + offset_bytes
    // 获取元素类型
    Type * elemType = arrType->getElementType();
    // 获取指针类型
    const Type * ptrType = PointerType::get(elemType);
    BinaryInstruction * addr_inst = new BinaryInstruction(module->getCurrentFunction(),
                                                          IRInstOperator::IRINST_OP_ADD_I,
                                                          base,
                                                          offset_bytes,
                                                          const_cast<Type *>(ptrType));
    node->blockInsts.addInst(addr_inst);

    // 判断是否需要降维，比如说a[3][3]，我调用a[3]
    int total_dims = dims.size();
    int used_dims = idx_cnt;
    if (used_dims < total_dims) {
        // 还有剩余维度，返回降维后的数组地址
        // 构造降维后的数组类型
        std::vector<int> remain_dims;
        for (int i = used_dims; i < total_dims; ++i)
            remain_dims.push_back(dims[i]);
        Type * subArrType = ArrayType::get(elemType, remain_dims);
        // addr_inst 的类型应为 subArrType 的指针
        // 这里可以考虑用 PointerType::get(subArrType)
        addr_inst->setType(subArrType);
        node->val = addr_inst;
        // 不生成load
        return true;
    } else {
        // 所有下标都给全，返回元素地址，getRValue时会生成load
        node->val = addr_inst;
        // return true;
    }

    // 判断是否有真假出口（如if/while条件）
    if (node->trueLabel && node->falseLabel) {
        // 1. 取值
        Value * addr = node->val;
        Value * base = module->findVarValue(node->sons[0]->name);
        ArrayType * arrType = dynamic_cast<ArrayType *>(base->getType());
        Type * elemType = arrType->getElementType();
        MemReadInstruction * load_inst = new MemReadInstruction(module->getCurrentFunction(), addr, elemType);
        node->blockInsts.addInst(load_inst);

        // 2. icmp ne %val, 0
        BinaryInstruction * cmp_inst = new BinaryInstruction(module->getCurrentFunction(),
                                                             IRInstOperator::IRINST_OP_NOT_EQ_I,
                                                             load_inst,
                                                             module->newConstInt(0),
                                                             IntegerType::getTypeBool());
        node->blockInsts.addInst(cmp_inst);

        // 3. 条件跳转
        node->blockInsts.addInst(new GotoInstruction(module->getCurrentFunction(),
                                                     cmp_inst,
                                                     static_cast<LabelInstruction *>(node->trueLabel),
                                                     static_cast<LabelInstruction *>(node->falseLabel)));

        node->val = cmp_inst; // 方便上层复用
        return true;
    } else {
        return true; // 没有真假出口，直接返回地址
    }
}

// 辅助函数：获取节点的右值（如果是数组访问则生成读内存指令）
Value * IRGenerator::getRValue(ast_node * node)
{
    // 如果是数组访问节点
    if (node->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
        if (!node->val) {
            fprintf(stderr, "getRValue: node->val is nullptr!\n");
            abort();
        }
        if (!node->sons[0]) {
            fprintf(stderr, "getRValue: node->sons[0] is nullptr!\n");
            abort();
        }
        // node->val 是地址
        Value * addr = node->val;
        // 获取元素类型
        Value * base = module->findVarValue(node->sons[0]->name);
        if (!base) {
            fprintf(stderr, "getRValue: base is nullptr! name=%s\n", node->sons[0]->name.c_str());
            abort();
        }
        ArrayType * arrType = dynamic_cast<ArrayType *>(base->getType());
        if (!arrType) {
            fprintf(stderr, "getRValue: arrType is nullptr!\n");
            abort();
        }
        Type * elemType = arrType->getElementType();

        // 生成读内存指令
        MemReadInstruction * readInst = new MemReadInstruction(module->getCurrentFunction(), addr, elemType);
        node->blockInsts.addInst(readInst);
        return readInst;
    }
    // 其它情况直接返回
    return node->val;
}