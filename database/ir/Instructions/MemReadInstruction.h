#pragma once
#include "Instruction.h"

/// @brief 内存读指令 result = *addr
class MemReadInstruction : public Instruction {
public:
    MemReadInstruction(Function * func, Value * addr, Type * type)
        : Instruction(func, IRInstOperator::IRINST_OP_MEM_READ, type)
    {
        addOperand(addr); // 操作数0：目标地址
    }

    void toString(std::string & str) override
    {
        str = getIRName() + " = *" + getOperand(0)->getIRName();
    }
};