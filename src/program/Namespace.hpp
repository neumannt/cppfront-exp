#ifndef H_Namespace
#define H_Namespace
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "program/Declaration.hpp"
#include <memory>
#include <unordered_map>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Declaration;
//---------------------------------------------------------------------------
/// A namespace
class Namespace {
    private:
    /// The name
    std::string name;
    /// All declarations within the namespace
    std::unordered_map<std::string, std::unique_ptr<Declaration>> declarations;

    public:
    /// Constructor
    Namespace(std::string name);
    /// Destructor
    ~Namespace();

    /// Find a declaration within the namespace
    Declaration* findDeclaration(std::string_view name);
    /// Create a new declaration
    Declaration* addDeclaration(SourceLocation loc, std::string_view name, bool isFunction);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
