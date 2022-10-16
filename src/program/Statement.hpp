#ifndef H_Statement
#define H_Statement
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Expression;
class Program;
//---------------------------------------------------------------------------
/// Base class for all statments
class Statement {
    public:
    /// Constructor
    Statement();
    /// Destructor
    virtual ~Statement();
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
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
