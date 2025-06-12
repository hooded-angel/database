///
/// @file GotoInstruction.h
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
#pragma once

#include <string>

#include "Instruction.h"
#include "LabelInstruction.h"
#include "Function.h"

///
/// @brief 无条件跳转指令
///
class GotoInstruction final : public Instruction {

public:
    /// 原有的无条件跳转
    /// @brief 无条件跳转指令的构造函数
    /// @param target 跳转目标
    ///
    GotoInstruction(Function * _func, Instruction * _target);

    // 新增：条件跳转
    // cond 非零时跳到 trueLabel，否则跳到 falseLabel
    GotoInstruction(Function * _func, Value * cond, LabelInstruction * trueLabel, LabelInstruction * falseLabel);

    /// @brief 转换成字符串
    void toString(std::string & str) override;

    /// @brief 获取目标Label指令
    /// @return LabelInstruction*
    [[nodiscard]] LabelInstruction * getTarget() const;

    /// 条件跳转时用于查询各字段
    [[nodiscard]] Value * getCond() const
    {
        return cond;
    }
    [[nodiscard]] LabelInstruction * getTrueLabel() const
    {
        return trueLabel;
    }
    [[nodiscard]] LabelInstruction * getFalseLabel() const
    {
        return falseLabel;
    }

private:
    /// 无条件跳转目标
    /// @brief 跳转到的目标Label指令
    LabelInstruction * target;

    // 条件跳转专用字段
    Value * cond = nullptr;
    LabelInstruction * trueLabel = nullptr;
    LabelInstruction * falseLabel = nullptr;
};
