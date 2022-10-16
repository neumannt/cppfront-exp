#ifndef H_Scope
#define H_Scope
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class FunctionType;
class Namespace;
//---------------------------------------------------------------------------
/// Anchor for all scope information, holds information that is global to the analysis
class ScopeRoot {
};
//---------------------------------------------------------------------------
/// Information about the current function (if any)
struct FunctionScope {
    /// The corresponding type
    const FunctionType* functionType;
};
//---------------------------------------------------------------------------
/// A (potentially nested) scope
class Scope {
    private:
    /// The root
    ScopeRoot& root;
    /// The parent scope (if any)
    Scope* parent;
    /// The current namespace
    Namespace* currentNamespace;
    /// The current function (if any)
    FunctionScope* currentFunction;

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
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
