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
    return iter->second.get();
}
//---------------------------------------------------------------------------
Declaration* Namespace::addDeclaration(SourceLocation loc, std::string_view name, bool isFunction)
// Create a new declaration
{
    auto n = make_unique<Declaration>(loc, string(name), isFunction);
    auto d = n.get();
    declarations.emplace(name, move(n));
    return d;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
