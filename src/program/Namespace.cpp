#include "program/Namespace.hpp"
#include "program/Declaration.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
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
Declaration* Namespace::findDeclaration(std::string_view name)
// Find a declaration within the namespace
{
    auto iter = declarations.find(string(name));
    if (iter == declarations.end()) return nullptr;
    return &(iter->second);
}
//---------------------------------------------------------------------------
Declaration* Namespace::addDeclaration(std::string_view name, bool isFunction)
// Create a new declaration
{
    return &(declarations.emplace(name, Declaration(string(name), isFunction)).first->second);
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
