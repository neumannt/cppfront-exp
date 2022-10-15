#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Declaration::Declaration(string name, bool isFunction)
    : name(move(name)), func(isFunction) {
}
//---------------------------------------------------------------------------
Declaration::~Declaration() {
}
//---------------------------------------------------------------------------
Declaration::Overload* Declaration::findFunctionOverload(const FunctionType* type)
// Check if an overload exists. This ignores parameter names and return types
{
    for (auto& o : overloads) {
        if (o.type == type) return &o;
        if (o.type->parameter.size() != type->parameter.size()) continue;
        bool match = true;
        for (unsigned index = 0, limit = type->parameter.size(); index != limit; ++index)
            if (o.type->parameter[index].type->isEquivalentTo(type->parameter[index].type)) {
                match = false;
                break;
            }
        if (match) return &o;
    }
    return nullptr;
}
//---------------------------------------------------------------------------
Declaration::Overload* Declaration::addFunctionOverload(const FunctionType* type, std::vector<std::unique_ptr<Expression>>&& defaultArguments, unsigned defaultArgumentsOffset)
// Add a new function overload
{
    overloads.emplace_back(type, move(defaultArguments), defaultArgumentsOffset);
    return &overloads.back();
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------