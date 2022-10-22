#ifndef H_CppOut
#define H_CppOut
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <iosfwd>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class BinaryExpression;
class Declaration;
class Expression;
class FunctionDeclaration;
class Program;
class Statement;
class Type;
//---------------------------------------------------------------------------
/// Logic for writing C++ code
class CppOut {
    /// The target
    std::ostream& out;
    /// The current position
    SourceLocation currentPos;
    /// Include line information?
    bool writeLines;
    /// Are we generating the body?
    bool inBody = false;

    /// Advance to a certain code location while pretty printing
    void advance(SourceLocation loc);
    /// Write a string
    void write(std::string_view s);
    /// Write strings
    void write(std::string_view a, std::string_view b);
    /// Write strings
    void write(std::string_view a, std::string_view b, std::string_view c);
    /// Write a type
    void writeType(const Type* type);

    /// Get the name of the return type of a declaration with multiple return values
    std::string returnTypeName(const FunctionDeclaration& decl, unsigned slot);

    /// Generate code for a declaration
    void generateDeclaration(const Declaration& decl, unsigned slot, bool inHeader);
    /// Generate code for a statement
    void generateStatement(const Statement& s);
    /// Generate code for a binary expression
    void generateBinaryExpression(const BinaryExpression& e);
    /// Generate code for an expression
    void generateExpression(const Expression& e);

    public:
    /// Constructor
    explicit CppOut(std::ostream& out, bool writeLines) : out(out), currentPos({"", 1, 1}), writeLines(writeLines) {}

    /// Generate the C++1 code
    void generate(const Program& prog);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
