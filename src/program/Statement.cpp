#include "program/Statement.hpp"
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
Statement::Statement()
// Constructor
{
}
//---------------------------------------------------------------------------
Statement::~Statement()
// Destructor
{
}
//---------------------------------------------------------------------------
CompoundStatement::~CompoundStatement()
// Destructor
{
}
//---------------------------------------------------------------------------
ReturnStatement::~ReturnStatement()
// Destructor
{
}
//---------------------------------------------------------------------------
ExpressionStatement::~ExpressionStatement()
// Destructor
{
}
//---------------------------------------------------------------------------
VariableStatement::~VariableStatement()
// Destructor
{
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
