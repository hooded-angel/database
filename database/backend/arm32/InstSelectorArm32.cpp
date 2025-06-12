///
/// @file InstSelectorArm32.cpp
/// @brief 指令选择器-ARM32的实现
/// @author zenglj (zenglj@live.com)
/// @version 1.0
/// @date 2024-11-21
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-11-21 <td>1.0     <td>zenglj  <td>新做
/// </table>
///
#include <cstdio>

#include "Common.h"
#include "ILocArm32.h"
#include "InstSelectorArm32.h"
#include "PlatformArm32.h"

#include "PointerType.h"
#include "RegVariable.h"
#include "Function.h"

#include "LabelInstruction.h"
#include "GotoInstruction.h"
#include "FuncCallInstruction.h"
#include "MoveInstruction.h"

/// @brief 构造函数
/// @param _irCode 指令
/// @param _iloc ILoc
/// @param _func 函数
InstSelectorArm32::InstSelectorArm32(vector<Instruction *> & _irCode,
                                     ILocArm32 & _iloc,
                                     Function * _func,
                                     SimpleRegisterAllocator & allocator)
    : ir(_irCode), iloc(_iloc), func(_func), simpleRegisterAllocator(allocator)
{
    translator_handlers[IRInstOperator::IRINST_OP_ENTRY] = &InstSelectorArm32::translate_entry;
    translator_handlers[IRInstOperator::IRINST_OP_EXIT] = &InstSelectorArm32::translate_exit;

    translator_handlers[IRInstOperator::IRINST_OP_LABEL] = &InstSelectorArm32::translate_label;
    translator_handlers[IRInstOperator::IRINST_OP_GOTO] = &InstSelectorArm32::translate_goto;

    translator_handlers[IRInstOperator::IRINST_OP_ASSIGN] = &InstSelectorArm32::translate_assign;

    // 算数运算
    translator_handlers[IRInstOperator::IRINST_OP_ADD_I] = &InstSelectorArm32::translate_add_int32;
    translator_handlers[IRInstOperator::IRINST_OP_SUB_I] = &InstSelectorArm32::translate_sub_int32;
    translator_handlers[IRInstOperator::IRINST_OP_MUL_I] = &InstSelectorArm32::translate_mul_int32;
    translator_handlers[IRInstOperator::IRINST_OP_DIV_I] = &InstSelectorArm32::translate_div_int32;
    translator_handlers[IRInstOperator::IRINST_OP_MOD_I] = &InstSelectorArm32::translate_mod_int32;

    // 取负运算
    translator_handlers[IRInstOperator::IRINST_OP_NEG_I] = &InstSelectorArm32::translate_neg_int32;

    // 关系运算
    translator_handlers[IRInstOperator::IRINST_OP_EQUAL_I] = &InstSelectorArm32::translate_icmp_eq;
    translator_handlers[IRInstOperator::IRINST_OP_NOT_EQ_I] = &InstSelectorArm32::translate_icmp_ne;
    translator_handlers[IRInstOperator::IRINST_OP_LESS_I] = &InstSelectorArm32::translate_icmp_lt;
    translator_handlers[IRInstOperator::IRINST_OP_LESS_EQ_I] = &InstSelectorArm32::translate_icmp_le;
    translator_handlers[IRInstOperator::IRINST_OP_GREATER_I] = &InstSelectorArm32::translate_icmp_gt;
    translator_handlers[IRInstOperator::IRINST_OP_GREATER_EQ_I] = &InstSelectorArm32::translate_icmp_ge;

    // 条件跳转
    translator_handlers[IRInstOperator::IRINST_OP_BC] = &InstSelectorArm32::translate_bc;

    translator_handlers[IRInstOperator::IRINST_OP_FUNC_CALL] = &InstSelectorArm32::translate_call;
    translator_handlers[IRInstOperator::IRINST_OP_ARG] = &InstSelectorArm32::translate_arg;
}

///
/// @brief 析构函数
///
InstSelectorArm32::~InstSelectorArm32()
{}

/// @brief 指令选择执行
void InstSelectorArm32::run()
{
    for (auto inst: ir) {

        // 逐个指令进行翻译
        if (!inst->isDead()) {
            translate(inst);
        }
    }
}

/// @brief 指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate(Instruction * inst)
{
    // 操作符
    IRInstOperator op = inst->getOp();

    map<IRInstOperator, translate_handler>::const_iterator pIter;
    pIter = translator_handlers.find(op);
    if (pIter == translator_handlers.end()) {
        // 没有找到，则说明当前不支持
        printf("Translate: Operator(%d) not support", (int) op);
        return;
    }

    // 开启时输出IR指令作为注释
    if (showLinearIR) {
        outputIRInstruction(inst);
    }

    (this->*(pIter->second))(inst);
}

///
/// @brief 输出IR指令
///
void InstSelectorArm32::outputIRInstruction(Instruction * inst)
{
    std::string irStr;
    inst->toString(irStr);
    if (!irStr.empty()) {
        iloc.comment(irStr);
    }
}

/// @brief NOP翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_nop(Instruction * inst)
{
    (void) inst;
    iloc.nop();
}

/// @brief Label指令指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_label(Instruction * inst)
{
    Instanceof(labelInst, LabelInstruction *, inst);

    iloc.label(labelInst->getName());
}

/// @brief goto指令指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_goto(Instruction * inst)
{
    Instanceof(gotoInst, GotoInstruction *, inst);

    // 无条件跳转
    iloc.jump(gotoInst->getTarget()->getName());
}

/// @brief 函数入口指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_entry(Instruction * inst)
{
    // 查看保护的寄存器
    auto & protectedRegNo = func->getProtectedReg();
    auto & protectedRegStr = func->getProtectedRegStr();

    bool first = true;
    for (auto regno: protectedRegNo) {
        if (first) {
            protectedRegStr = PlatformArm32::regName[regno];
            first = false;
        } else {
            protectedRegStr += "," + PlatformArm32::regName[regno];
        }
    }

    if (!protectedRegStr.empty()) {
        iloc.inst("push", "{" + protectedRegStr + "}");
    }

    // 为fun分配栈帧，含局部变量、函数调用值传递的空间等
    iloc.allocStack(func, ARM32_TMP_REG_NO);
}

/// @brief 函数出口指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_exit(Instruction * inst)
{
    if (inst->getOperandsNum()) {
        // 存在返回值
        Value * retVal = inst->getOperand(0);

        // 赋值给寄存器R0
        iloc.load_var(0, retVal);
    }

    // 恢复栈空间
    iloc.inst("mov", "sp", "fp");

    // 保护寄存器的恢复
    auto & protectedRegStr = func->getProtectedRegStr();
    if (!protectedRegStr.empty()) {
        iloc.inst("pop", "{" + protectedRegStr + "}");
    }

    iloc.inst("bx", "lr");
}

/// @brief 赋值指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_assign(Instruction * inst)
{
    Value * result = inst->getOperand(0);
    Value * arg1 = inst->getOperand(1);

    int32_t arg1_regId = arg1->getRegId();
    int32_t result_regId = result->getRegId();

    if (arg1_regId != -1) {
        // 寄存器 => 内存
        // 寄存器 => 寄存器

        // r8 -> rs 可能用到r9
        iloc.store_var(arg1_regId, result, ARM32_TMP_REG_NO);
    } else if (result_regId != -1) {
        // 内存变量 => 寄存器

        iloc.load_var(result_regId, arg1);
    } else {
        // 内存变量 => 内存变量

        int32_t temp_regno = simpleRegisterAllocator.Allocate();

        // arg1 -> r8
        iloc.load_var(temp_regno, arg1);

        // r8 -> rs 可能用到r9
        iloc.store_var(temp_regno, result, ARM32_TMP_REG_NO);

        simpleRegisterAllocator.free(temp_regno);
    }
}

/// @brief 二元操作指令翻译成ARM32汇编
/// @param inst IR指令
/// @param operator_name 操作码
/// @param rs_reg_no 结果寄存器号
/// @param op1_reg_no 源操作数1寄存器号
/// @param op2_reg_no 源操作数2寄存器号
void InstSelectorArm32::translate_two_operator(Instruction * inst, string operator_name)
{
    Value * result = inst;
    Value * arg1 = inst->getOperand(0);
    Value * arg2 = inst->getOperand(1);

    int32_t arg1_reg_no = arg1->getRegId();
    int32_t arg2_reg_no = arg2->getRegId();
    int32_t result_reg_no = inst->getRegId();
    int32_t load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no;

    // 看arg1是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg1_reg_no == -1) {

        // 分配一个寄存器r8
        load_arg1_reg_no = simpleRegisterAllocator.Allocate(arg1);

        // arg1 -> r8，这里可能由于偏移不满足指令的要求，需要额外分配寄存器
        iloc.load_var(load_arg1_reg_no, arg1);
    } else {
        load_arg1_reg_no = arg1_reg_no;
    }

    // 看arg2是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg2_reg_no == -1) {

        // 分配一个寄存器r9
        load_arg2_reg_no = simpleRegisterAllocator.Allocate(arg2);

        // arg2 -> r9
        iloc.load_var(load_arg2_reg_no, arg2);
    } else {
        load_arg2_reg_no = arg2_reg_no;
    }

    // 看结果变量是否是寄存器，若不是则需要分配一个新的寄存器来保存运算的结果
    if (result_reg_no == -1) {
        // 分配一个寄存器r10，用于暂存结果
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // r8 + r9 -> r10
    iloc.inst(operator_name,
              PlatformArm32::regName[load_result_reg_no],
              PlatformArm32::regName[load_arg1_reg_no],
              PlatformArm32::regName[load_arg2_reg_no]);

    // 结果不是寄存器，则需要把rs_reg_name保存到结果变量中
    if (result_reg_no == -1) {

        // 这里使用预留的临时寄存器，因为立即数可能过大，必须借助寄存器才可操作。

        // r10 -> result
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg1);
    simpleRegisterAllocator.free(arg2);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数加法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_add_int32(Instruction * inst)
{
    translate_two_operator(inst, "add");
}

/// @brief 整数减法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_sub_int32(Instruction * inst)
{
    translate_two_operator(inst, "sub");
}

/// @brief 整数乘法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_mul_int32(Instruction * inst)
{
    translate_two_operator(inst, "mul");
}

/// @brief 整数除法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_div_int32(Instruction * inst)
{
    translate_two_operator(inst, "sdiv");
}

/// @brief 整数取模指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_mod_int32(Instruction * inst)
{
    // IR 中的 result = arg1 % arg2
    Value * result = inst;
    Value * dividend = inst->getOperand(0); // 被除数
    Value * divisor = inst->getOperand(1);  // 除数

    int32_t reg_dividend = dividend->getRegId();
    int32_t reg_divisor = divisor->getRegId();
    int32_t reg_result = result->getRegId();

    int32_t r_div, r_dvs, r_res;
    int32_t r_q, r_mul;

    // 准备 dividend 寄存器
    if (reg_dividend == -1) {
        r_div = simpleRegisterAllocator.Allocate(dividend);
        iloc.load_var(r_div, dividend);
    } else {
        r_div = reg_dividend;
    }

    // 准备 divisor 寄存器
    if (reg_divisor == -1) {
        r_dvs = simpleRegisterAllocator.Allocate(divisor);
        iloc.load_var(r_dvs, divisor);
    } else {
        r_dvs = reg_divisor;
    }

    // 准备 result 寄存器（用于存放最终的余数）
    if (reg_result == -1) {
        r_res = simpleRegisterAllocator.Allocate(result);
    } else {
        r_res = reg_result;
    }

    // 分配一个临时寄存器用于存放 quotient
    r_q = simpleRegisterAllocator.Allocate();

    // 执行除法： r_q = dividend / divisor
    iloc.inst("sdiv", PlatformArm32::regName[r_q], PlatformArm32::regName[r_div], PlatformArm32::regName[r_dvs]);

    // 分配另一个临时寄存器用于 r_q * divisor
    r_mul = simpleRegisterAllocator.Allocate();
    iloc.inst("mul", PlatformArm32::regName[r_mul], PlatformArm32::regName[r_q], PlatformArm32::regName[r_dvs]);

    // 计算余数： r_res = dividend - (quotient * divisor)
    iloc.inst("sub", PlatformArm32::regName[r_res], PlatformArm32::regName[r_div], PlatformArm32::regName[r_mul]);

    // 如果 result 不是寄存器变量，需要写回内存
    if (reg_result == -1) {
        iloc.store_var(r_res, result, ARM32_TMP_REG_NO);
    }

    // 释放所有分配的寄存器
    simpleRegisterAllocator.free(dividend);
    simpleRegisterAllocator.free(divisor);
    simpleRegisterAllocator.free(result);
    simpleRegisterAllocator.free(r_q);
    simpleRegisterAllocator.free(r_mul);
}

/// @brief 整数取负指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_neg_int32(Instruction * inst)
{
    Value * result = inst;
    Value * arg = inst->getOperand(0);

    int32_t arg_reg_no = arg->getRegId();
    int32_t result_reg_no = result->getRegId();
    int32_t load_arg_reg_no, load_result_reg_no;

    // 获取操作数寄存器
    if (arg_reg_no == -1) {
        load_arg_reg_no = simpleRegisterAllocator.Allocate(arg);
        iloc.load_var(load_arg_reg_no, arg);
    } else {
        load_arg_reg_no = arg_reg_no;
    }

    // 获取结果寄存器
    if (result_reg_no == -1) {
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // rsb Rd, Rn, #0 相当于 Rd = 0 - Rn
    iloc.inst("rsb", PlatformArm32::regName[load_result_reg_no], PlatformArm32::regName[load_arg_reg_no], "#0");

    // 如果结果不是寄存器，保存回内存
    if (result_reg_no == -1) {
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数等于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_eq(Instruction * inst)
{
    Value * result = inst;
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();
    int32_t res_reg = result->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }
    if (res_reg == -1) {
        res_reg = simpleRegisterAllocator.Allocate(result);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);
    iloc.inst("moveq", PlatformArm32::regName[res_reg], "#1");
    iloc.inst("movne", PlatformArm32::regName[res_reg], "#0");

    if (result->getRegId() == -1) {
        iloc.store_var(res_reg, result, ARM32_TMP_REG_NO);
    }

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数不等于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_ne(Instruction * inst)
{
    Value * result = inst;
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();
    int32_t res_reg = result->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }
    if (res_reg == -1) {
        res_reg = simpleRegisterAllocator.Allocate(result);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);
    iloc.inst("moveq", PlatformArm32::regName[res_reg], "#0");
    iloc.inst("movne", PlatformArm32::regName[res_reg], "#1");

    if (result->getRegId() == -1) {
        iloc.store_var(res_reg, result, ARM32_TMP_REG_NO);
    }

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数小于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_lt(Instruction * inst)
{
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
}

/// @brief 整数小于等于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_le(Instruction * inst)
{
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
}

/// @brief 整数大于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_gt(Instruction * inst)
{
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
}

/// @brief 整数大于等于比较指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_icmp_ge(Instruction * inst)
{
    Value * lhs = inst->getOperand(0);
    Value * rhs = inst->getOperand(1);

    int32_t lhs_reg = lhs->getRegId();
    int32_t rhs_reg = rhs->getRegId();

    if (lhs_reg == -1) {
        lhs_reg = simpleRegisterAllocator.Allocate(lhs);
        iloc.load_var(lhs_reg, lhs);
    }
    if (rhs_reg == -1) {
        rhs_reg = simpleRegisterAllocator.Allocate(rhs);
        iloc.load_var(rhs_reg, rhs);
    }

    iloc.inst("cmp", PlatformArm32::regName[lhs_reg], PlatformArm32::regName[rhs_reg]);

    simpleRegisterAllocator.free(lhs);
    simpleRegisterAllocator.free(rhs);
}

void InstSelectorArm32::translate_bc(Instruction * inst)
{
    // bc cond, label_true, label_false
    Value * cond = inst->getOperand(0);
    Value * label_true = inst->getOperand(1);
    Value * label_false = inst->getOperand(2);

    // 获取 cond 的定义指令
    Instruction * defInst = dynamic_cast<Instruction *>(cond);

    // 默认跳转指令
    std::string branch_inst = "bne";
    // 如果 cond 的定义指令不是关系运算（如 icmp/eq/ne/lt/gt）
    // 而是普通算术（如 neg、add、sub），应在跳转前加一条 cmp cond_reg, #0
    bool need_cmp = true;

    if (defInst) {
        switch (defInst->getOp()) {
            case IRInstOperator::IRINST_OP_LESS_I:
                branch_inst = "blt";
                need_cmp = false;
                break;
            case IRInstOperator::IRINST_OP_LESS_EQ_I:
                branch_inst = "ble";
                need_cmp = false;
                break;
            case IRInstOperator::IRINST_OP_GREATER_I:
                branch_inst = "bgt";
                need_cmp = false;
                break;
            case IRInstOperator::IRINST_OP_GREATER_EQ_I:
                branch_inst = "bge";
                need_cmp = false;
                break;
            case IRInstOperator::IRINST_OP_EQUAL_I:
                branch_inst = "beq";
                need_cmp = false;
                break;
            case IRInstOperator::IRINST_OP_NOT_EQ_I:
                branch_inst = "bne";
                need_cmp = false;
                break;
            default:
                branch_inst = "bne";
                need_cmp = true;
                break;
        }
    }

    int cond_reg = cond->getRegId();
    if (cond_reg == -1) {
        cond_reg = simpleRegisterAllocator.Allocate(cond);
        iloc.load_var(cond_reg, cond);
    }

    // 只有不是关系运算时才需要cmp
    if (need_cmp) {
        iloc.inst("cmp", PlatformArm32::regName[cond_reg], "#0");
    }

    // 条件跳转指令
    iloc.inst(branch_inst, label_true->getName());
    iloc.inst("b", label_false->getName());

    simpleRegisterAllocator.free(cond);
}

/// @brief 函数调用指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_call(Instruction * inst)
{
    FuncCallInstruction * callInst = dynamic_cast<FuncCallInstruction *>(inst);

    int32_t operandNum = callInst->getOperandsNum();

    if (operandNum != realArgCount) {

        // 两者不一致 也可能没有ARG指令，正常
        if (realArgCount != 0) {

            minic_log(LOG_ERROR, "ARG指令的个数与调用函数个数不一致");
        }
    }

    if (operandNum) {

        // 强制占用这几个寄存器参数传递的寄存器
        simpleRegisterAllocator.Allocate(0);
        simpleRegisterAllocator.Allocate(1);
        simpleRegisterAllocator.Allocate(2);
        simpleRegisterAllocator.Allocate(3);

        // 前四个的后面参数采用栈传递
        int esp = 0;
        for (int32_t k = 4; k < operandNum; k++) {

            auto arg = callInst->getOperand(k);

            // 新建一个内存变量，用于栈传值到形参变量中
            MemVariable * newVal = func->newMemVariable((Type *) PointerType::get(arg->getType()));
            newVal->setMemoryAddr(ARM32_SP_REG_NO, esp);
            esp += 4;

            Instruction * assignInst = new MoveInstruction(func, newVal, arg);

            // 翻译赋值指令
            translate_assign(assignInst);

            delete assignInst;
        }

        for (int32_t k = 0; k < operandNum && k < 4; k++) {

            auto arg = callInst->getOperand(k);

            // 检查实参的类型是否是临时变量。
            // 如果是临时变量，该变量可更改为寄存器变量即可，或者设置寄存器号
            // 如果不是，则必须开辟一个寄存器变量，然后赋值即可

            Instruction * assignInst = new MoveInstruction(func, PlatformArm32::intRegVal[k], arg);

            // 翻译赋值指令
            translate_assign(assignInst);

            delete assignInst;
        }
    }

    iloc.call_fun(callInst->getName());

    if (operandNum) {
        simpleRegisterAllocator.free(0);
        simpleRegisterAllocator.free(1);
        simpleRegisterAllocator.free(2);
        simpleRegisterAllocator.free(3);
    }

    // 赋值指令
    if (callInst->hasResultValue()) {

        // 新建一个赋值操作
        Instruction * assignInst = new MoveInstruction(func, callInst, PlatformArm32::intRegVal[0]);

        // 翻译赋值指令
        translate_assign(assignInst);

        delete assignInst;
    }

    // 函数调用后清零，使得下次可正常统计
    realArgCount = 0;
}

///
/// @brief 实参指令翻译成ARM32汇编
/// @param inst
///
void InstSelectorArm32::translate_arg(Instruction * inst)
{
    // 翻译之前必须确保源操作数要么是寄存器，要么是内存，否则出错。
    Value * src = inst->getOperand(0);

    // 当前统计的ARG指令个数
    int32_t regId = src->getRegId();

    if (realArgCount < 4) {
        // 前四个参数
        if (regId != -1) {
            if (regId != realArgCount) {
                // 肯定寄存器分配有误
                minic_log(LOG_ERROR, "第%d个ARG指令对象寄存器分配有误: %d", argCount + 1, regId);
            }
        } else {
            minic_log(LOG_ERROR, "第%d个ARG指令对象不是寄存器", argCount + 1);
        }
    } else {
        // 必须是内存分配，若不是则出错
        int32_t baseRegId;
        bool result = src->getMemoryAddr(&baseRegId);
        if ((!result) || (baseRegId != ARM32_SP_REG_NO)) {

            minic_log(LOG_ERROR, "第%d个ARG指令对象不是SP寄存器寻址", argCount + 1);
        }
    }

    realArgCount++;
}
