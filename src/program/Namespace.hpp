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
    /// The parent namespace (if any)
    Namespace* parent;
    /// All declarations within the namespace
    std::unordered_map<DeclarationId, std::unique_ptr<Declaration>> declarations;
    /// The depth from the root
    unsigned depth;

    public:
    /// Constructor
    Namespace(std::string name, Namespace* parent);
    /// Destructor
    virtual ~Namespace();

    /// Find a declaration within the namespace
    Declaration* findDeclaration(const DeclarationId& name) const;
    /// Create a new declaration
    Declaration* addDeclaration(std::unique_ptr<Declaration> decl);

    /// Get the name
    auto& getName() const { return name; }
    /// Get the parent namespace
    Namespace* getParent() const { return parent; }

    /// Get the depth from the root
    unsigned getDepth() const { return depth; }
    /// Get the path step at a certain depth
    std::string_view getPathStep(unsigned depth) const;
    /// Find the lowest common ancestor of two namespace
    unsigned findLCA(const Namespace* other) const;
};
//---------------------------------------------------------------------------
/// A class is a special type of namespace
class Class : public Namespace {
    /// The new type
    std::unique_ptr<Type> type;

    public:
    /// Constructor
    Class(std::string name, Namespace* parent, Program* program);
    /// Destructor
    ~Class();

    /// Find a function declaration within the class or a base class
    FunctionDeclaration* findWithInheritance(const DeclarationId& name) const;

    /// Get the class type
    const Type* getType() const { return type.get(); }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
