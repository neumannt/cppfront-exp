#ifndef H_Statement
#define H_Statement
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Expression;
class Program;
class Type;
//---------------------------------------------------------------------------
/// Base class for all statments
class Statement {
    public:
    /// Types of statements
    enum class Type {
        Compound,
        Variable,
        Return,
        Expression
    };

    /// Constructor
    Statement();
    /// Destructor
    virtual ~Statement();

    /// Get the statement type
    virtual Type getType() const = 0;
};
//---------------------------------------------------------------------------
/// A compound statement
class CompoundStatement : public Statement {
    /// The original source location (for pretty printing)
    SourceLocation begin, end;
    /// The contained statements
    std::vector<std::unique_ptr<Statement>> statements;

    public:
    /// Constructor
    CompoundStatement(SourceLocation begin, SourceLocation end, std::vector<std::unique_ptr<Statement>>&& statements) : begin(begin), end(end), statements(std::move(statements)) {}
    /// Destructor
    ~CompoundStatement();

    /// Get the statement type
    Type getType() const override { return Type::Compound; }
    /// Get the begin location
    auto getBegin() const { return begin; }
    /// Get the end location
    auto getEnd() const { return end; }
    /// Get the contained statements
    auto& getStatements() const { return statements; }
};
//---------------------------------------------------------------------------
/// A return statement
class ReturnStatement : public Statement {
    /// The original source location (for pretty printing)
    SourceLocation begin;
    /// The value to return (if any)
    std::unique_ptr<Expression> exp;

    public:
    /// Constructor
    ReturnStatement(SourceLocation begin, std::unique_ptr<Expression>&& exp) : begin(begin), exp(std::move(exp)) {}
    /// Destructor
    ~ReturnStatement();

    /// Get the statement type
    Type getType() const override { return Type::Return; }
    /// Get the begin location
    auto getBegin() const { return begin; }
    /// Access the value to return (if any)
    auto& getExpression() const { return exp; }
};
//---------------------------------------------------------------------------
/// An expression statement
class ExpressionStatement : public Statement {
    /// The original source location (for pretty printing)
    SourceLocation begin;
    /// The value to return (if any)
    std::unique_ptr<Expression> exp;

    public:
    /// Constructor
    ExpressionStatement(SourceLocation begin, std::unique_ptr<Expression>&& exp) : begin(begin), exp(std::move(exp)) {}
    /// Destructor
    ~ExpressionStatement();

    /// Get the statement type
    Type getType() const override { return Type::Expression; }
    /// Get the begin location
    auto getBegin() const { return begin; }
    /// Access the value to return (if any)
    auto& getExpression() const { return exp; }
};
//---------------------------------------------------------------------------
/// A variable statement
class VariableStatement : public Statement {
    /// The original source location (for pretty printing)
    SourceLocation begin;
    /// The variable name
    std::string name;
    /// The type
    const cpp2exp::Type* type;
    /// The initial value (if any)
    std::unique_ptr<Expression> init;

    public:
    /// Constructor
    VariableStatement(SourceLocation begin, std::string name, const cpp2exp::Type* type, std::unique_ptr<Expression>&& init) : begin(begin), name(std::move(name)), type(type), init(std::move(init)) {}
    /// Destructor
    ~VariableStatement();

    /// Get the statement type
    Type getType() const override { return Type::Variable; }
    /// Get the begin location
    auto getBegin() const { return begin; }
    /// Get the name
    auto& getName() const { return name; }
    /// Get the type
    auto getDeclType() const { return type; }
    /// Access the init value (if any)
    auto& getInit() const { return init; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
