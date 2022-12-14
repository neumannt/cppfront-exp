#include "codegen/CppOut.hpp"
#include "parser/Parser.hpp"
#include "semana/SemanticAnalysis.hpp"
#include <fstream>
#include <iostream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
static void printErrors(const vector<cpp2exp::Error>& errors) {
    for (auto& e : errors) {
        cout << e.loc.file << ":" << e.loc.line << ":" << e.loc.column << ":" << e.text << endl;
    }
}
//---------------------------------------------------------------------------
static void showHelp(const char* argv0) {
    cerr << "experimental compiler for cpp2, functionality is incomplete" << endl;
    cerr << "usage: " << argv0 << "[-o filename] <input.cpp2>" << endl;
}
//---------------------------------------------------------------------------
static bool checkMessage(const vector<cpp2exp::Error>& errors, const string& message) {
    if (message.empty()) return true;
    for (auto& e : errors)
        if (e.text.find(message) != string::npos)
            return true;
    cerr << "expected error message: " << message << endl
         << "got:" << endl;
    printErrors(errors);
    return false;
}
//---------------------------------------------------------------------------
int main(int argc, char* argv[]) {
    string outfile;
    vector<string> files;
    bool argsMode = false, testMode = false, clean = false;
    string includeDir = "include";
    for (int index = 1; index != argc; ++index) {
        string_view a = argv[index];
        if (argsMode) {
            files.push_back(string(a));
        } else {
            char c = (a.size() > 0) ? a[0] : 0;
            if (c == '-') {
                if (a == "--") {
                    argsMode = true;
                    continue;
                }
                if ((a == "-h") || (a == "--help")) {
                    showHelp(argv[0]);
                    return 0;
                }
                if (a == "-o") {
                    if ((!outfile.empty()) || (index + 1 >= argc)) {
                        showHelp(argv[0]);
                        return 1;
                    }
                    outfile = argv[++index];
                    continue;
                }
                if (a == "--test") {
                    testMode = true;
                    continue;
                }
                if (a == "-c") {
                    clean = true;
                    continue;
                }
                if (a[1] == 'I') {
                    includeDir = a.substr(2);
                    continue;
                }
                cout << "invalid option " << a << endl;
                return 1;
            }
            files.push_back(string(a));
        }
    }
    if (files.empty()) {
        showHelp(argv[0]);
        return 1;
    }
    if ((!outfile.empty()) && (files.size() > 1)) {
        cerr << "-o is only supported for a single input file for now" << endl;
        return 1;
    }
    static_cast<void>(testMode);

    for (auto fileName : files) {
        string outname;
        if (outfile.empty()) {
            outname = (fileName.rfind('.') == string::npos) ? fileName + ".cpp" : (fileName.substr(0, fileName.rfind('.')) + ".cpp");
        } else {
            outname = outfile;
        }

        cpp2exp::Parser parser;
        const cpp2exp::AST* ast;
        ast = parser.parseFile(fileName);

        // Interpret test annotations
        enum class Mode { Success,
                          ParsingFails,
                          AnalysisFails };
        Mode mode = Mode::Success;
        string expected;
        if (testMode) {
            auto sep = parser.getContent().find('\n');
            string statement(parser.getContent().substr(0, sep));
            if (statement == "//test ok") {
                mode = Mode::Success;
            } else if (statement == "//test failure parsing") {
                mode = Mode::ParsingFails;
            } else if (statement == "//test failure compiling") {
                mode = Mode::AnalysisFails;
            } else {
                cerr << "test annotation missing for " << fileName << endl;
                return 1;
            }
            if (parser.getContent().substr(sep + 1, 10) == "//message ") {
                expected = parser.getContent().substr(sep + 11, parser.getContent().find('\n', sep + 1) - (sep + 11));
            }
        }

        if (mode != Mode::ParsingFails) {
            if (!ast) {
                printErrors(parser.getErrors());
                return 1;
            } else {
                if (!testMode)
                    cout << "parsing ok" << endl;
            }
        } else {
            if (ast) {
                cerr << "parsing " << fileName << " should fail, but doesn't" << endl;
                return 1;
            }
            if (!checkMessage(parser.getErrors(), expected)) return 1;
            continue;
        }

        cpp2exp::SemanticAnalysis semana(parser.getFileName(), parser.getContent());
        if (!semana.loadStdlib()) {
            printErrors(semana.getErrors());
            return 1;
        }
        if (!semana.analyze(ast)) {
            if (mode != Mode::AnalysisFails) {
                printErrors(semana.getErrors());
                return 1;
            } else {
                if (!checkMessage(semana.getErrors(), expected)) return 1;
                continue;
            }
        } else {
            if (mode == Mode::AnalysisFails) {
                cerr << "compiling " << fileName << " should fail, but doesn't" << endl;
                return 1;
            }
            if (!testMode)
                cout << "semantic analysis ok" << endl;
        }

        {
            ofstream f(outname);
            cpp2exp::CppOut out(f, !clean);
            out.generate(semana.getProgram(), semana.getTarget());
        }

        if (testMode) {
            // In test mode we make sure the result compiles with gcc
            string cmd = "g++ --std=c++20 -I" + includeDir + " -c -o/dev/null " + outname;
            if (system(cmd.c_str()) != 0) {
                cout << "compilation failed" << endl;
                return 1;
            }
        }
    }

    return 0;
}
//---------------------------------------------------------------------------
