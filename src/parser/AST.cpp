#include "AST.hpp"
#include "Lexer.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
void* ASTContainer::allocateRaw(unsigned)
// Allocate memory for a node
{
    return nullptr; // TODO
}
//---------------------------------------------------------------------------
namespace ast {
//---------------------------------------------------------------------------
Token::Token(std::string_view content, unsigned fromLine, unsigned fromColumn, unsigned toLine, unsigned toColumn)
    : content(content), fromLine(fromLine), fromColumn(fromColumn), toLine(toLine), toColumn(toColumn) {
}
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
