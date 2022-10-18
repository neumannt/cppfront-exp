#ifndef H_Expression
#define H_Expression
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Type;
//---------------------------------------------------------------------------
/// Base class for all expressions
class Expression {
    public:
    /// Expression categories
    enum class Category {
        Literal
    };

    protected:
    /// The original position in the source code
    SourceLocation loc;
    /// The result type
    const Type* type;

    /// Constructor
    Expression(SourceLocation loc, const Type* type) : loc(loc), type(type) {}

    public:
    /// Destructor
    virtual ~Expression();

    /// Get the expression category
    virtual Category getCategory() const = 0;
    /// Get the result type
    const Type* getType() const { return type; }
};
//---------------------------------------------------------------------------
/// A literal
class Literal : public Expression {
    protected:
    /// The text
    std::string_view text;

    public:
    /// Constructor
    Literal(SourceLocation loc, const Type* type, std::string_view text) : Expression(loc, type), text(text) {}

    /// Get the expression category
    Category getCategory() const override { return Category::Literal; }
    /// Get the text
    auto getText() const { return text; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
