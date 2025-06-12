#pragma once
#include "Instruction.h"
#include "VoidType.h"

/// @brief 内存写指令 *addr = value
class MemWriteInstruction : public Instruction {
public:
    MemWriteInstruction(Function * func, Value * addr, Value * value)
        : Instruction(func, IRInstOperator::IRINST_OP_MEM_WRITE, VoidType::getType())
    {
        addOperand(addr);  // 操作数0：目标地址
        addOperand(value); // 操作数1：要写入的值
    }

    void toString(std::string & str) override
    {
        str = "*" + getOperand(0)->getIRName() + " = " + getOperand(1)->getIRName();
    }
};