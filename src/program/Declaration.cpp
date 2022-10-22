#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
#include "program/Namespace.hpp"
#include "program/Statement.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Declaration::Declaration(SourceLocation loc, string name)
    : loc(loc), name(move(name)) {
}
//---------------------------------------------------------------------------
Declaration::~Declaration() {
}
//---------------------------------------------------------------------------
VariableDeclaration::VariableDeclaration(SourceLocation loc, string name)
    : Declaration(loc, move(name))
// Constructor
{
}
//---------------------------------------------------------------------------
VariableDeclaration::~VariableDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
FunctionDeclaration::FunctionDeclaration(SourceLocation loc, string name)
    : Declaration(loc, move(name))
// Constructor
{
}
//---------------------------------------------------------------------------
FunctionDeclaration::~FunctionDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
FunctionDeclaration::Overload* FunctionDeclaration::findFunctionOverload(const FunctionType* type)
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
unsigned FunctionDeclaration::addFunctionOverload(SourceLocation loc, const FunctionType* type, std::vector<std::unique_ptr<Expression>>&& defaultArguments, unsigned defaultArgumentsOffset)
// Add a new function overload
{
    overloads.emplace_back(loc, type, move(defaultArguments), defaultArgumentsOffset);
    return overloads.size() - 1;
}
//---------------------------------------------------------------------------
NamespaceDeclaration::NamespaceDeclaration(SourceLocation loc, string name)
    : Declaration(loc, move(name))
// Constructor
{
}
//---------------------------------------------------------------------------
NamespaceDeclaration::~NamespaceDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
