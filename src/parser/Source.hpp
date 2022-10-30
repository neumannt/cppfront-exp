#ifndef H_Source
#define H_Source
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/AST.hpp"
#include <string_view>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
struct AST;
//---------------------------------------------------------------------------
/// A source for the parser. Provided convenience functions for the action rules
class Source {
    /// The AST container
    ASTContainer& container;
    /// The file name
    std::string_view fileName;
    /// The content
    std::string_view content;
    /// The current position
    unsigned pos;

    /// Create an AST node
    AST* astImpl(AST::Type type, unsigned subType, Range range, std::initializer_list<const AST*> args, unsigned flags = 0);

    public:
    /// Constructor
    Source(ASTContainer& container, std::string_view fileName, std::string_view content) : container(container), fileName(fileName), content(content), pos(0) {}

    /// Read the next character
    int getChar() { return (pos < content.length()) ? (content[pos++] & 0xFF) : -1; }

    /// Report an unknown error (due to incomplete grammar)
    int reportUnknownError();
    /// Report an error
    int reportError(const char* message, Range pos);

    /// Create an AST node
    AST* ast(AST::Type type, Range range);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range);
    /// Create an AST node
    template <class T>
    requires(std::is_enum_v<T>)
        AST* ast2(AST::Type type, T subType, Range range) { return ast2(type, static_cast<unsigned>(subType), range); }
    /// Create an AST node
    AST* ast(AST::Type type, Range range, const AST* a1);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range, const AST* a1);
    /// Create an AST node
    AST* ast(AST::Type type, Range range, const AST* a1, const AST* a2);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2);
    /// Create an AST node
    AST* ast(AST::Type type, Range range, const AST* a1, const AST* a2, const AST* a3);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2, const AST* a3);
    /// Create an AST node
    AST* ast(AST::Type type, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4);
    /// Create an AST node
    AST* ast(AST::Type type, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4, const AST* a5);
    /// Create an AST node
    AST* ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4, const AST* a5);

    /// Build a list with a single element
    AST* buildList(AST::Type type, Range range, const AST* element);
    /// Append to a list
    AST* appendList(AST::Type type, Range range, AST* list, const AST* element);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
