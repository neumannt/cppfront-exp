#ifndef H_AST
#define H_AST
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// Base class for AST nodes
class AST {
};
//---------------------------------------------------------------------------
/// Container for managing AST nodes
class ASTContainer {
    public:
    /// Allocate memory for an AST node
    void* allocateRaw(unsigned size);
};
//---------------------------------------------------------------------------
namespace ast {
//---------------------------------------------------------------------------
/// A terminal token
class Token : public AST {
    public:
    /// The content
    std::string_view content;
    /// Position in text
    unsigned fromLine, fromColumn, toLine, toColumn;

    /// Constructor
    Token(std::string_view content, unsigned fromLine, unsigned fromColumn, unsigned toLine, unsigned toColumn);
};
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
#endif
