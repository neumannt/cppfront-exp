#include "program/Declaration.hpp"
#include "infra/Hash.hpp"
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
std::size_t std::hash<cpp2exp::DeclarationId>::operator()(const cpp2exp::DeclarationId& id) const noexcept {
    return cpp2exp::Hash::hash({static_cast<unsigned>(id.category), cpp2exp::Hash::hashString(id.name)});
}
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Declaration::Declaration(SourceLocation loc, DeclarationId name)
    : loc(loc), name(move(name)) {
}
//---------------------------------------------------------------------------
Declaration::~Declaration() {
}
//---------------------------------------------------------------------------
const Type* Declaration::getCorrespondingType() const
// Get the corresponding type (if any)
{
    return nullptr;
}
//---------------------------------------------------------------------------
VariableDeclaration::VariableDeclaration(SourceLocation loc, DeclarationId name, const Type* type)
    : Declaration(loc, move(name)), type(type)
// Constructor
{
}
//---------------------------------------------------------------------------
VariableDeclaration::~VariableDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
FunctionDeclaration::FunctionDeclaration(SourceLocation loc, DeclarationId name)
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
NamespaceDeclaration::NamespaceDeclaration(SourceLocation loc, DeclarationId name, Namespace* parent)
    : Declaration(loc, name), ns(make_unique<Namespace>(name.name, parent))
// Constructor
{
}
//---------------------------------------------------------------------------
NamespaceDeclaration::~NamespaceDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
ClassDeclaration::ClassDeclaration(SourceLocation loc, DeclarationId name, Namespace* parent, Program* program)
    : Declaration(loc, name), cl(make_unique<Class>(name.name, parent, program))
// Constructor
{
}
//---------------------------------------------------------------------------
ClassDeclaration::~ClassDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
const Type* ClassDeclaration::getCorrespondingType() const
// Get the corresponding type (if any)
{
    return cl->getType();
}
//---------------------------------------------------------------------------
TypedefDeclaration::TypedefDeclaration(SourceLocation loc, DeclarationId name, const Type* originalType)
    : Declaration(loc, move(name)), newType(make_unique<AliasType>(originalType->getProgram(), originalType))
// Constructor
{
}
//---------------------------------------------------------------------------
TypedefDeclaration::~TypedefDeclaration()
// Destructor
{
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
