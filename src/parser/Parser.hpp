#ifndef H_Parser
#define H_Parser
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "AST.hpp"
#include "Lexer.hpp"
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// An error description
struct Error {
    /// The occurrence in the source
    Lexer::SourceLocation loc;
    /// The text
    std::string text;
};
//---------------------------------------------------------------------------
/// A parser
class Parser {
    public:
    using SourceLocation = Lexer::SourceLocation;

    private:
    /// The content
    std::string content;
    /// The AST nodes
    ASTContainer astContainer;
    /// The errors
    std::vector<Error> errors;

    /// Store an error message
    void addError(Lexer::SourceLocation loc, std::string text);

    /// The bison interface
    const AST* parseImpl();

    public:
    /// Constructor
    Parser();
    /// Destructor
    ~Parser();

    /// Load the input from a file and parse the file
    const AST* parseFile(const std::string& fileName);
    /// Get the error list
    auto& getErrors() const { return errors; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
