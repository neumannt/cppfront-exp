#include "program/Namespace.hpp"
#include "program/Declaration.hpp"
#include "program/Type.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Namespace::Namespace(string name, Namespace* parent)
    : name(move(name)), parent(parent), depth(parent ? (parent->depth + 1) : 0) {
}
//---------------------------------------------------------------------------
Namespace::~Namespace() {
}
//---------------------------------------------------------------------------
Declaration* Namespace::findDeclaration(const DeclarationId& name) const
// Find a declaration within the namespace
{
    auto iter = declarations.find(name);
    if (iter == declarations.end()) return nullptr;
    return iter->second.get();
}
//---------------------------------------------------------------------------
Declaration* Namespace::addDeclaration(std::unique_ptr<Declaration> decl)
// Create a new declaration
{
    auto name = decl->getName();
    auto d = decl.get();
    declarations.emplace(name, move(decl));
    return d;
}
//---------------------------------------------------------------------------
string_view Namespace::getPathStep(unsigned depth) const
// Get the path step at a certain depth
{
    auto iter = this;
    while (iter->depth > depth) iter = iter->parent;
    return iter->name;
}
//---------------------------------------------------------------------------
unsigned Namespace::findLCA(const Namespace* other) const
// Find the lowest common ancestor of two namespace
{
    auto a = this, b = other;
    while (a->depth > b->depth) a = a->parent;
    while (b->depth > a->depth) b = b->parent;
    while (a != b) {
        a = a->parent;
        b = b->parent;
    }
    return a->depth;
}
//---------------------------------------------------------------------------
Class::Class(std::string name, Namespace* parent, Program* program)
    : Namespace(std::move(name), parent), type(make_unique<ClassType>(program, this))
// Constructor
{}
//---------------------------------------------------------------------------
Class::~Class()
// Destructor
{
}
//---------------------------------------------------------------------------
FunctionDeclaration* Class::findWithInheritance(const DeclarationId& name) const
// Find a declaration within the class or a blase class
{
    // TODO handle inheritance
    auto d = findDeclaration(name);
    if (d && d->isFunction()) return static_cast<FunctionDeclaration*>(d);
    return nullptr;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
