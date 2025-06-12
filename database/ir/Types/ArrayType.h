#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <cstdint>
#include <sstream>
#include "Type.h"

///
/// @brief 数组类型描述类，支持多维数组
///
class ArrayType : public Type {
public:
    /// @brief 获取或创建一个数组类型
    /// @param elemType 元素类型
    /// @param dims 每一维的长度（如[4,2]表示int[4][2]）
    static ArrayType * get(Type * elemType, const std::vector<int> & dims)
    {
        if (!elemType) {
            std::cerr << "[ArrayType::get] elemType is nullptr!" << std::endl;
            abort();
        }
        return new ArrayType(elemType, dims);
    }

    /// @brief 获取元素类型
    [[nodiscard]] Type * getElementType() const
    {
        return elemType;
    }

    /// @brief 获取所有维度
    [[nodiscard]] const std::vector<int> & getDims() const
    {
        return dims;
    }

    /// @brief 获取数组总元素个数
    [[nodiscard]] int getTotalElementCount() const
    {
        int total = 1;
        for (int d: dims)
            total *= d;
        return total;
    }

    /// @brief 获取类型所占内存空间大小
    [[nodiscard]] int32_t getSize() const override
    {
        return getTotalElementCount() * elemType->getSize();
    }

    /// @brief 类型字符串，如i32[4][2]
    [[nodiscard]] std::string toString() const override
    {
        std::ostringstream oss;
        oss << elemType->toString();
        for (int d: dims) {
            oss << "[" << d << "]";
        }
        return oss.str();
    }

    /// @brief 判断是否为数组类型
    [[nodiscard]] bool isArrayType() const override
    {
        return true;
    }

protected:
    ArrayType(Type * elemType, const std::vector<int> & dims) : Type(Type::ArrayTyID), elemType(elemType), dims(dims)
    {
        if (!elemType) {
            std::cerr << "[ArrayType ctor] elemType is nullptr!" << std::endl;
            abort();
        }
    }

    Type * elemType;
    std::vector<int> dims;
};