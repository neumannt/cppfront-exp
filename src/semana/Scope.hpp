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
class AST;
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
/// Possible control flow states
enum class ControlFlow : unsigned {
    Normal,
    Returns,
    Throws,
    ReturnsOrThrows
};
//---------------------------------------------------------------------------
/// A (potentially nested) scope
class Scope {
    public:
    /// A variable
    struct Var {
        /// The type
        const Type* type;
        /// The scope level
        unsigned level;
        /// Initialized?
        bool initialized;
        /// Wrapped type?
        bool wrapped;
    };
    /// Information about initialized variables
    struct InitInfo {
        /// The AST entry (for error reporting)
        const AST* ast;
        /// The original variable
        Var* var;
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
    /// All initialized variables from outer scopes
    std::vector<InitInfo> initializedVars;
    /// The level
    unsigned level;

    public:
    /// Constructor
    explicit Scope(ScopeRoot& root) : root(root), parent(nullptr), currentNamespace(nullptr), currentFunction(nullptr), level(0) {}
    /// Constructor
    explicit Scope(Scope& parent) : root(parent.root), parent(&parent), currentNamespace(parent.currentNamespace), currentFunction(parent.currentFunction), level(parent.level + 1) {}

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

    /// Mark a variable as initialized
    void markInitialized(const AST* ast, Var* var);
    /// Get the initialized vars from outer scopes
    auto& getInitializedVars() const { return initializedVars; }
    /// Propagate initialization information up
    void resolve(ControlFlow cf);
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
