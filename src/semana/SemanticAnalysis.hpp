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
class Expression;
class FunctionType;
class Module;
class Namespace;
class Program;
class Type;
//---------------------------------------------------------------------------
/// Semantic analysis logic
class SemanticAnalysis {
    private:
    /// The mapping logic
    RangeMapping mapping;
    /// The errors
    std::vector<Error> errors;
    /// The program
    std::unique_ptr<Program> program;
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

    /// Analyze and expression
    std::unique_ptr<Expression> analyzeExpression(const AST* ast);
    /// Analyze an id-expression with optional pointer markers
    const Type* analyzeIdExpression(const AST* ast);
    /// Analyze an id-expression with optional pointer markers
    const Type* analyzeTypeIdExpression(const AST* ast);
    /// Analyze an unnamed declaration
    bool analyzeUnnamedDeclaration(const AST* ast, const Type** type, std::unique_ptr<Expression>* value);
    /// Analyze a function type declaration
    const FunctionType* analyzeFunctionType(const AST* ast, std::vector<std::unique_ptr<Expression>>* defaultArguments, unsigned* defaultArgumentsOffset);
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
