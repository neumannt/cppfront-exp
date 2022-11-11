#ifndef H_Scope
#define H_Scope
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class FunctionType;
class Namespace;
class Type;
//---------------------------------------------------------------------------
/// Anchor for all scope information, holds information that is global to the analysis
class ScopeRoot {
};
//---------------------------------------------------------------------------
struct FunctionScope;
//---------------------------------------------------------------------------
/// A (potentially nested) scope
class Scope {
    public:
    /// A variable
    struct Var {
        /// The type
        const Type* type;
        /// Initialized?
        bool initialized;
        /// Wrapped type?
        bool wrapped;
    };

    private:
    /// The root
    ScopeRoot& root;
    /// The parent scope (if any)
    Scope* parent;
    /// The current namespace
    Namespace* currentNamespace;
    /// The current function (if any)
    FunctionScope* currentFunction;
    /// All variable definitions
    std::unordered_map<std::string, Var> variables;

    public:
    /// Constructor
    explicit Scope(ScopeRoot& root) : root(root), parent(nullptr), currentNamespace(nullptr), currentFunction(nullptr) {}
    /// Constructor
    explicit Scope(Scope& parent) : root(parent.root), parent(&parent), currentNamespace(parent.currentNamespace), currentFunction(parent.currentFunction) {}

    /// Get the current namespace
    Namespace* getCurrentNamespace() const { return currentNamespace; }
    /// Switch the current namespace
    void setCurrentNamespace(Namespace* n) { currentNamespace = n; }
    /// Get the current function
    FunctionScope* getCurrentFunction() const { return currentFunction; }
    /// Switch the current function
    void setCurrentFunction(FunctionScope* f) { currentFunction = f; }

    /// Is a variable defined in this scope here?
    bool definesVariable(const std::string& name) const;
    /// Define a variable
    Var* defineVariable(const std::string& name, const Type* type, bool uninitialized, bool wrapped);
    /// Resolve a variable in this or in parent scopes
    Var* resolveVariable(const std::string& name);
};
//---------------------------------------------------------------------------
/// Information about the current function (if any)
struct FunctionScope {
    /// The corresponding type
    const FunctionType* functionType;
    /// The out parameter (if any)
    std::vector<std::pair<std::string, Scope::Var*>> out;
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
