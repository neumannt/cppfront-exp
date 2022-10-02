#include "parser/Lexer.hpp"
#include <iostream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
static void printErrors(string_view fileName, const vector<cpp2exp::Lexer::Error>& errors) {
    for (auto& e : errors) {
        cout << fileName << ":" << e.loc.line << ":" << e.loc.column << ":" << e.text << endl;
    }
}
//---------------------------------------------------------------------------
int main(int argc, char* argv[]) {
    if (argc != 2) {
        cout << "usage: " << argv[0] << " input" << endl;
        return 1;
    }
    cpp2exp::Lexer lexer;
    if (!lexer.loadFile(argv[1])) {
        printErrors(argv[1], lexer.getErrors());
        return 1;
    }
    while (lexer.next() != cpp2exp::Lexer::Token::Eof) {}
    if (!lexer.getErrors().empty()) {
        printErrors(argv[1], lexer.getErrors());
    } else {
        cout << "lexing ok" << endl;
    }
    return 0;
}
//---------------------------------------------------------------------------
