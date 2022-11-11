#include "semana/Scope.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
bool Scope::definesVariable(const std::string& name) const
// Is a variable defined in this scope here?
{
    return variables.count(name);
}
//---------------------------------------------------------------------------
Scope::Var* Scope::defineVariable(const std::string& name, const Type* type, bool uninitialized, bool wrapped)
// Define a variable
{
    variables[name] = {type, !uninitialized, wrapped};
    return &(variables[name]);
}
//---------------------------------------------------------------------------
Scope::Var* Scope::resolveVariable(const std::string& name)
/// Resolve a variable in this or in parent scopes
{
    for (auto scope = this; scope; scope = scope->parent) {
        if (auto iter = scope->variables.find(name); iter != scope->variables.end()) return &(iter->second);
    }
    return nullptr;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
