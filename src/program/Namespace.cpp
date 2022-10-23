#include "program/Namespace.hpp"
#include "program/Declaration.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Namespace::Namespace(string name)
    : name(move(name)) {
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
}
//---------------------------------------------------------------------------
