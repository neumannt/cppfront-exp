#include "program/Expression.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Expression::~Expression()
// Destructor
{
}
//---------------------------------------------------------------------------
Expression::Precedence BinaryExpression::getPrecedence() const
// Get the expression precedence (for printing)
{
    switch (getOp()) {
        case Op::LogicalAnd: return Precedence::LogicalAndExpression;
        case Op::LogicalOr: return Precedence::LogicalOrExpression;
        case Op::BitAnd: return Precedence::BitAnd;
        case Op::BitOr: return Precedence::BitOr;
        case Op::BitXor: return Precedence::BitXor;
        case Op::Equal: return Precedence::Equality;
        case Op::NotEqual: return Precedence::Equality;
        case Op::Less: return Precedence::Relational;
        case Op::LessEq: return Precedence::Relational;
        case Op::Greater: return Precedence::Relational;
        case Op::GreaterEq: return Precedence::Relational;
        case Op::Spaceship: return Precedence::Compare;
        case Op::LeftShift: return Precedence::Shift;
        case Op::RightShift: return Precedence::Shift;
        case Op::Plus: return Precedence::Additive;
        case Op::Minus: return Precedence::Additive;
        case Op::Mul: return Precedence::Multiplicative;
        case Op::Div: return Precedence::Multiplicative;
        case Op::Modulo: return Precedence::Multiplicative;
    }
    return Precedence::Primary; // unreachable
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
