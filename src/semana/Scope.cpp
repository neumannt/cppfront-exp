#include "semana/Scope.hpp"
#include <cassert>
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
    variables[name] = {type, level, !uninitialized, wrapped};
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
void Scope::markInitialized(const AST* ast, Var* var)
// Mark a variable as initialized
{
    assert(!var->initialized);
    var->initialized = true;
    if (var->level < level) initializedVars.push_back({ast, var});
}
//---------------------------------------------------------------------------
void Scope::resolve(ControlFlow cf)
// Propagate initialization information up
{
    if (cf == ControlFlow::Normal) {
        // Propagate the initializations that are relevant for the outer scope
        if (parent) {
            for (auto& i : initializedVars)
                if (i.var->level < parent->level)
                    parent->initializedVars.push_back(i);
        }
    } else {
        // Control flow does not reach the end of the scope, reset the flags
        for (auto& i : initializedVars) i.var->initialized = false;
    }
    initializedVars.clear();
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
