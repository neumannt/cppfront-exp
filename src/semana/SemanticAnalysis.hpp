#ifndef H_SemanticAnalysis
#define H_SemanticAnalysis
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class AST;
class Expression;
class Declaration;
class DeclarationId;
class FunctionDeclaration;
class FunctionType;
class Literal;
class Parser;
class Module;
class Namespace;
class Program;
class Statement;
class Scope;
class Type;
enum class ValueCategory : unsigned;
enum class ControlFlow : unsigned;
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
    /// The mocked stdlib
    std::unique_ptr<Parser> stdlibMock;
    /// Are we currently processing the mocked stdlib?
    bool inStdlib = false;

    /// Store an error message and throw an exception
    [[noreturn]] void throwError(SourceLocation loc, std::string text);
    /// Store an error message
    [[noreturn]] void throwError(const AST* loc, std::string text);

    /// Access the content of a node
    std::string_view accessText(const AST* ast);
    /// Extract an identifier
    std::string_view extractIdentifier(const AST* ast);
    /// Extract a function id
    DeclarationId extractDeclarationId(const AST* ast);
    /// Make sure an expression is convertible into a certain type
    void enforceConvertible(const AST* loc, std::unique_ptr<Expression>& exp, const Type* target, bool explicitScope = false);
    /// Try to resolve an operator
    std::pair<const Type*, ValueCategory> resolveOperator(Scope& scope, const AST* ast, const DeclarationId& id, const Expression& left, const Expression& right);
    /// Resolve an unqualified id
    Declaration* resolveUnqualifiedId(Scope& scope, const AST* ast);
    /// Resolve a qualified id
    Declaration* resolveQualifiedId(Scope& scope, const AST* ast);
    /// Derive a constexpr value
    std::unique_ptr<Literal> deriveConstexpr(const Expression& exp);

    /// A call argument
    struct CallArg {
        /// The modifiers
        enum Modifier {
            Regular,
            Out,
            Move
        };

        /// The type
        const Type* type;
        /// The value category
        ValueCategory category;
        /// The modifier (if any)
        Modifier modifier;
    };
    /// Resolve the function to call
    std::optional<std::pair<FunctionDeclaration*, unsigned>> resolveCall(const AST* ast, std::span<FunctionDeclaration*> candidates, std::span<const CallArg> args, bool reportErrors = true);

    /// The result of a statement
    struct StatementResult {
        /// The statement itself
        std::unique_ptr<Statement> statement;
        /// The control flow state
        ControlFlow state;
    };

    /// Analyze an expression
    std::unique_ptr<Expression> analyzeExpression(Scope& scope, const AST* ast, const Type* typeHint = nullptr);
    /// Analyze a literal expression
    std::unique_ptr<Expression> analyzeLiteral(const AST* ast);
    /// Analyze an assignment expression
    std::unique_ptr<Expression> analyzeAssignmentExpression(Scope& scope, const AST* ast);
    /// Analyze a binary expression
    std::unique_ptr<Expression> analyzeBinaryExpression(Scope& scope, const AST* ast);
    /// Analyze an expression list expression
    std::unique_ptr<Expression> analyzeExpressionListExpression(Scope& scope, const AST* ast, const Type* typeHint);
    /// Analyze an id-expression that is part of an expression
    std::unique_ptr<Expression> analyzeIdExpressionExpression(Scope& scope, const AST* ast);
    /// Analyze a declaration statement
    StatementResult analyzeDeclarationStatement(Scope& scope, const AST* ast);
    /// Analyze a compound statement
    StatementResult analyzeCompoundStatement(Scope& scope, const AST* ast);
    /// Analyze a return statement
    StatementResult analyzeReturnStatement(Scope& scope, const AST* ast);
    /// Analyze a selection statement
    StatementResult analyzeSelectionStatement(Scope& scope, const AST* ast);
    /// Analyze an expression statement
    StatementResult analyzeExpressionStatement(Scope& scope, const AST* ast);
    /// Analyze a statement
    StatementResult analyzeStatement(Scope& scope, const AST* ast);
    /// Analyze an id-expression with optional pointer markers
    const Type* analyzeIdExpression(Scope& scope, const AST* ast);
    /// Analyze an id-expression with optional pointer markers
    const Type* analyzeTypeIdExpression(Scope& scope, const AST* ast);
    /// Analyze an unnamed declaration
    void analyzeUnnamedDeclaration(Scope& scope, const AST* ast, const Type** type, std::unique_ptr<Expression>* value);
    /// Analyze a function type declaration
    const FunctionType* analyzeFunctionType(Scope& scope, const AST* ast, std::vector<std::unique_ptr<Expression>>* defaultArguments, unsigned* defaultArgumentsOffset, unsigned* declarationFlags);
    /// Analyze a declaration
    void analyzeDeclaration(Scope& scope, const AST* declaration);
    /// Analyze a definition (ignore declarations
    void analyzeDefinition(Scope& scope, const AST* definition);
    /// Phases of analysis
    enum class Phase : bool { Declarations,
                              Definitions };
    /// Analyze a namespace
    void analyzeNamespace(Scope& scope, const AST* ast, Phase phase);
    /// Analyze a class definition
    void analyzeClass(Scope& scope, const AST* ast, Phase phase);
    /// Analyze a typedef
    void analyzeUsing(Scope& scope, const AST* ast, Phase phase, bool usingDecltype);
    /// Analyze all declarations
    void analyzeDeclarations(Scope& scope, const AST* declarations, Phase phase);

    public:
    /// Constructor
    SemanticAnalysis(std::string_view fileName, std::string_view content);
    /// Destructor
    ~SemanticAnalysis();

    /// Load the C++1 stdlib mock
    bool loadStdlib();

    /// Analyze a parse tree
    bool analyze(const AST* translationUnit);
    /// Get the error list
    auto& getErrors() const { return errors; }
    /// Get the program
    auto& getProgram() const { return *program; }
    /// Get the target
    auto& getTarget() const { return *target; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
