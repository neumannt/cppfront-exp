#ifndef H_SemanticAnalysis
#define H_SemanticAnalysis
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class AST;
class Module;
class Namespace;
//---------------------------------------------------------------------------
/// Semantic analysis logic
class SemanticAnalysis {
    private:
    /// The mapping logic
    RangeMapping mapping;
    /// The errors
    std::vector<Error> errors;
    /// The target
    std::unique_ptr<Module> target;
    /// The current namespace
    Namespace* currentNamespace;

    /// Store an error message
    void addError(SourceLocation loc, std::string text);
    /// Store an error message
    bool addError(const AST* loc, std::string text);

    /// Access the content of a node
    std::string_view accessText(const AST* ast);
    /// Extract an identifier
    std::string_view extractIdentifier(const AST* ast);

    /// Analyze a declaration
    bool analyzeDeclaration(const AST* declaration);
    /// Analyze a definition (ignore declarations
    bool analyzeDefinition(const AST* definition);

    public:
    /// Constructor
    SemanticAnalysis(std::string_view fileName, std::string_view content);
    /// Destructor
    ~SemanticAnalysis();

    /// Analyze a parse tree
    bool analyze(const AST* translationUnit);
    /// Get the error list
    auto& getErrors() const { return errors; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
