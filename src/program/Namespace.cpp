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
    : name(move(name)), parent(parent) {
}
//---------------------------------------------------------------------------
Namespace::~Namespace() {
}
//---------------------------------------------------------------------------
Declaration* Namespace::findDeclaration(const DeclarationId& name)
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
}
//---------------------------------------------------------------------------
