#ifndef H_Namespace
#define H_Namespace
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
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
    std::unordered_map<DeclarationId, std::unique_ptr<Declaration>> declarations;

    public:
    /// Constructor
    Namespace(std::string name);
    /// Destructor
    ~Namespace();

    /// Find a declaration within the namespace
    Declaration* findDeclaration(const DeclarationId& name);
    /// Create a new declaration
    Declaration* addDeclaration(std::unique_ptr<Declaration> decl);
};
//---------------------------------------------------------------------------
/// A class is a special type of namespace
class Class : public Namespace {
    public:
    /// Constructor
    Class(std::string name) : Namespace(std::move(name)) {}
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
