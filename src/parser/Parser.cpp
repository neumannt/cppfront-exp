#include "Parser.hpp"
#include <fstream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
Parser::Parser()
// Constructor
{
}
//---------------------------------------------------------------------------
Parser::~Parser()
// Destructor
{
}
//---------------------------------------------------------------------------
void Parser::addError(SourceLocation loc, string text)
// Store an error message
{
    errors.emplace_back(loc, move(text));
}
//---------------------------------------------------------------------------
const AST* Parser::parseFile(const string& fileName)
// Load the input from a file and parse it
{
    {
        // Load the file into memory
        ifstream in(fileName);
        if (!in.is_open()) {
            addError({}, "unable top open " + fileName);
            return nullptr;
        }
        content = string(istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
    }

    // And parse
    return parseImpl();
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
