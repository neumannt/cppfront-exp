#ifndef H_Expression
#define H_Expression
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Type;
class VariableDeclaration;
//---------------------------------------------------------------------------
/// The value category
enum class ValueCategory : unsigned {
    Prvalue,
    Xvalue,
    Lvalue
};
//---------------------------------------------------------------------------
/// Base class for all expressions
class Expression {
    public:
    /// Expression categories
    enum class Category {
        Literal,
        Binary,
        Variable
    };
    /// Precedence levels
    enum class Precedence {
        LogicalOrExpression,
        LogicalAndExpression,
        BitOr,
        BitXor,
        BitAnd,
        Equality,
        Relational,
        Compare,
        Shift,
        Additive,
        Multiplicative,
        Primary
    };
    using ValueCategory = cpp2exp::ValueCategory;
    /// The characteristics of an expression
    struct Characteristics {
        /// The type
        const Type* type;
        /// The value category
        ValueCategory category;
    };

    protected:
    /// The original position in the source code
    SourceLocation loc;
    /// The result type
    const Type* type;
    /// The value category
    ValueCategory valueCategory;

    /// Constructor
    Expression(SourceLocation loc, const Type* type, ValueCategory valueCategory) : loc(loc), type(type), valueCategory(valueCategory) {}

    public:
    /// Destructor
    virtual ~Expression();

    /// Get the expression category
    virtual Category getCategory() const = 0;
    /// Get the expression precedence (for printing)
    virtual Precedence getPrecedence() const = 0;
    /// Get the original source location
    SourceLocation getLocation() const { return loc; }
    /// Get the result type
    const Type* getType() const { return type; }
    /// Get the value category
    ValueCategory getValueCategory() const { return valueCategory; }
    /// Get the characteristics of the expression
    Characteristics getCharacteristics() const { return {getType(), getValueCategory()}; }
};
//---------------------------------------------------------------------------
/// A literal
class Literal : public Expression {
    protected:
    /// The text
    std::string_view text;

    public:
    /// Constructor
    Literal(SourceLocation loc, const Type* type, std::string_view text) : Expression(loc, type, ValueCategory::Prvalue), text(text) {}

    /// Get the expression category
    Category getCategory() const override { return Category::Literal; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override { return Precedence::Primary; }
    /// Get the text
    auto getText() const { return text; }
};
//---------------------------------------------------------------------------
/// A binary expression
class BinaryExpression : public Expression {
    public:
    /// The operation
    enum Op {
        LogicalAnd,
        LogicalOr,
        BitAnd,
        BitOr,
        BitXor,
        Equal,
        NotEqual,
        Less,
        LessEq,
        Greater,
        GreaterEq,
        Spaceship,
        LeftShift,
        RightShift,
        Plus,
        Minus,
        Mul,
        Div,
        Modulo
    };

    private:
    /// The operation
    Op op;
    /// The input
    std::unique_ptr<Expression> left, right;

    public:
    /// Constructor
    BinaryExpression(SourceLocation loc, const Type* type, ValueCategory valueCategory, Op op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) : Expression(loc, type, valueCategory), op(op), left(std::move(left)), right(std::move(right)) {}

    /// Get the operation
    Op getOp() const { return op; }
    /// Get the left input
    const Expression& getLeft() const { return *left; }
    /// Get the right input
    const Expression& getRight() const { return *right; }

    /// Get the expression category
    Category getCategory() const override { return Category::Binary; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override;
};
//---------------------------------------------------------------------------
/// A variable reference
class VariableExpression : public Expression {
    protected:
    /// The variable
    VariableDeclaration* decl;

    public:
    /// Constructor
    VariableExpression(SourceLocation loc, const Type* type, VariableDeclaration* decl) : Expression(loc, type, ValueCategory::Lvalue), decl(decl) {}

    /// Get the expression category
    Category getCategory() const override { return Category::Variable; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override { return Precedence::Primary; }
    /// Get the variable
    auto getVariable() const { return decl; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
