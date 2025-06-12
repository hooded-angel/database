///
/// @file GotoInstruction.cpp
/// @brief 无条件跳转指令即goto指令
///
/// @author zenglj (zenglj@live.com)
/// @version 1.0
/// @date 2024-09-29
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-09-29 <td>1.0     <td>zenglj  <td>新建
/// </table>
///

#include "VoidType.h"

#include "GotoInstruction.h"

/// 无条件跳转指令的构造函数
/// @brief 无条件跳转指令的构造函数
/// @param target 跳转目标
GotoInstruction::GotoInstruction(Function * _func, Instruction * _target)
    : Instruction(_func, IRInstOperator::IRINST_OP_GOTO, VoidType::getType())
{
    // 真假目标一样，则无条件跳转
    target = static_cast<LabelInstruction *>(_target);
}

/// 条件跳转构造：cond!=0 则跳 trueLabel，否则跳 falseLabel
GotoInstruction::GotoInstruction(Function * _func,
                                 Value * _cond,
                                 LabelInstruction * _trueLabel,
                                 LabelInstruction * _falseLabel)
    : Instruction(_func, IRInstOperator::IRINST_OP_BC, VoidType::getType()), cond(_cond), trueLabel(_trueLabel),
      falseLabel(_falseLabel)
{
    // 有条件跳转时 target 字段不使用
    target = nullptr;
    // 把操作数加到 operands 容器里
    this->addOperand(_cond);
    this->addOperand(_trueLabel);
    this->addOperand(_falseLabel);
}

/// @brief 转换成IR指令文本
void GotoInstruction::toString(std::string & str)
{
    // str = "br label " + target->getIRName();
    if (cond) {
        // 有条件跳转
        str = "bc ";
        str += cond->getIRName();
        str += ", label ";
        str += trueLabel->getIRName();
        str += ", label ";
        str += falseLabel->getIRName();
    } else {
        // 无条件跳转
        str = "br label " + target->getIRName();
    }
}

///
/// @brief 获取目标Label指令
/// @return LabelInstruction* label指令
///
LabelInstruction * GotoInstruction::getTarget() const
{
    return target;
}
