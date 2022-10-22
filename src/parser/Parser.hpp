#ifndef H_Parser
#define H_Parser
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/AST.hpp"
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// A parser
class Parser {
    private:
    /// The file name
    std::string fileName;
    /// The content
    std::string content;
    /// The AST nodes
    ASTContainer astContainer;
    /// The errors
    std::vector<Error> errors;

    /// Store an error message
    void addError(SourceLocation loc, std::string text);

    /// The bison interface
    const AST* parseImpl();

    public:
    /// Constructor
    Parser();
    /// Destructor
    ~Parser();

    /// Load the input from a file and parse the file
    const AST* parseFile(const std::string& fileName);
    /// Load the input from a string and it
    const AST* parseString(const std::string& fileName, std::string_view input);
    /// Get the error list
    auto& getErrors() const { return errors; }
    /// Get the file name
    std::string_view getFileName() const { return fileName; }
    /// Get the content of the translation unit
    std::string_view getContent() const { return content; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
