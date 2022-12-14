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
class Namespace;
class Type;
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
        Unary,
        Binary,
        Assignment,
        Variable,
        WrappedVariable
    };
    /// Precedence levels
    enum class Precedence {
        Assignment,
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
        Unary,
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
/// A unary expression
class UnaryExpression : public Expression {
    public:
    /// The operation
    enum Op {
        Not,
        Plus,
        Minus
    };

    private:
    /// The operation
    Op op;
    /// The input
    std::unique_ptr<Expression> input;

    public:
    /// Constructor
    UnaryExpression(SourceLocation loc, const Type* type, ValueCategory valueCategory, Op op, std::unique_ptr<Expression> input) : Expression(loc, type, valueCategory), op(op), input(std::move(input)) {}

    /// Get the operation
    Op getOp() const { return op; }
    /// Get the input
    const Expression& getInput() const { return *input; }

    /// Get the expression category
    Category getCategory() const override { return Category::Unary; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override;
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
/// An assignment expression
class AssignmentExpression : public Expression {
    public:
    /// The operation
    enum Op {
        Construct,
        Assignment,
        BitOrEq,
        BitAndEq,
        BitXorEq,
        MulEq,
        DivEq,
        ModuloEq,
        PlusEq,
        MinusEq,
        LeftShiftEq,
        RightShiftEq
    };

    private:
    /// The operation
    Op op;
    /// The input
    std::unique_ptr<Expression> left, right;

    public:
    /// Constructor
    AssignmentExpression(SourceLocation loc, const Type* type, ValueCategory valueCategory, Op op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) : Expression(loc, type, valueCategory), op(op), left(std::move(left)), right(std::move(right)) {}

    /// Get the operation
    Op getOp() const { return op; }
    /// Get the left input
    const Expression& getLeft() const { return *left; }
    /// Get the right input
    const Expression& getRight() const { return *right; }

    /// Get the expression category
    Category getCategory() const override { return Category::Assignment; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override;
};
//---------------------------------------------------------------------------
/// A variable reference
class VariableExpression : public Expression {
    protected:
    /// The name
    std::string name;
    /// The containing namespace (nullptr for local variables)
    Namespace* containingNamespace;

    public:
    /// Constructor
    VariableExpression(SourceLocation loc, const Type* type, std::string name, Namespace* containingNamespace) : Expression(loc, type, ValueCategory::Lvalue), name(std::move(name)), containingNamespace(containingNamespace) {}

    /// Get the expression category
    Category getCategory() const override { return Category::Variable; }
    /// Get the expression precedence (for printing)
    Precedence getPrecedence() const override { return Precedence::Primary; }
    /// Get the name
    auto& getName() const { return name; }
    /// Get the containing namespace
    auto getContainingNamespace() const { return containingNamespace; }
};
//---------------------------------------------------------------------------
/// A wrapped variable reference
class WrappedVariableExpression : public VariableExpression {
    public:
    using VariableExpression::VariableExpression;

    /// Get the expression category
    Category getCategory() const override { return Category::WrappedVariable; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
