#include "codegen/CppOut.hpp"
#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
#include "program/Program.hpp"
#include "program/Statement.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
void CppOut::advance(SourceLocation loc)
// Advance to a certain code location while pretty printing
{
    if (inBody && (currentPos.line < loc.line)) {
        while (currentPos.line < loc.line) {
            write("\n");
            currentPos.line++;
            currentPos.column = 1;
        }
        while (currentPos.column < loc.column) {
            write(" ");
            currentPos.column++;
        }
    }
}
//---------------------------------------------------------------------------
void CppOut::write(std::string_view s)
// Write a string
{
    // We use a dedicated function here because we might want to keep track of the layout for pretty printing
    out << s;
}
//---------------------------------------------------------------------------
void CppOut::write(std::string_view a, std::string_view b)
// Write strings
{
    write(a);
    write(b);
}
//---------------------------------------------------------------------------
void CppOut::write(std::string_view a, std::string_view b, std::string_view c)
// Write strings
{
    write(a);
    write(b);
    write(c);
}
//---------------------------------------------------------------------------
void CppOut::writeType(const Type* type)
// Write a type
{
    switch (type->getCategory()) {
        case Type::Category::Fundamental:
            using FundamentalTypeId = FundamentalType::FundamentalTypeId;
            switch (static_cast<const FundamentalType*>(type)->getId()) {
                case FundamentalTypeId::Void: write("void"); break;
                case FundamentalTypeId::Char: write(is_unsigned_v<char> ? "signed char" : "char"); break;
                case FundamentalTypeId::UnsignedChar: write("unsigned char"); break;
                case FundamentalTypeId::Char8: write("char8_t"); break;
                case FundamentalTypeId::Char16: write("char16_t"); break;
                case FundamentalTypeId::Char32: write("char32_t"); break;
                case FundamentalTypeId::WChar: write("wchar_t"); break;
                case FundamentalTypeId::Short: write("short"); break;
                case FundamentalTypeId::UnsignedShort: write("unsigned short"); break;
                case FundamentalTypeId::Int: write("int"); break;
                case FundamentalTypeId::UnsignedInt: write("unsigned int"); break;
                case FundamentalTypeId::Long: write("long"); break;
                case FundamentalTypeId::UnsignedLong: write("unsigned long"); break;
                case FundamentalTypeId::LongLong: write("long long"); break;
                case FundamentalTypeId::UnsignedLongLong: write("unsigned long long"); break;
                case FundamentalTypeId::Bool: write("bool"); break;
                case FundamentalTypeId::Float: write("float"); break;
                case FundamentalTypeId::Double: write("double"); break;
                case FundamentalTypeId::LongDouble: write("long double"); break;
                case FundamentalTypeId::NullptrType: write("std::nullptr_t"); break;
            }
            break;
        case Type::Category::Function: write("??functiontype??"); break; // TODO
        case Type::Category::Pointer:
            writeType(static_cast<const PointerType*>(type)->getElementType());
            write("*");
            break;
    }
}
//---------------------------------------------------------------------------
std::string CppOut::returnTypeName(const Declaration& decl, unsigned slot)
// Get the name of the return type of a declaration with multiple return values
{
    if (decl.getOverloadCount() > 1)
        return "return_" + string(decl.getName()) + "_" + to_string(slot);
    return "return_" + string(decl.getName());
}
//---------------------------------------------------------------------------
void CppOut::generateDeclaration(const Declaration& decl, unsigned slot, bool inHeader)
// Generate code for a declaration
{
    if (decl.isFunction()) {
        auto& o = decl.accessOverload(slot);
        advance(o.loc);
        auto type = o.type;
        if (type->returnValues.empty()) {
            write("[[nodiscard]] void");
        } else if ((type->returnValues.size() == 1) && (type->returnValues[0].first.empty())) {
            writeType(type->returnValues[0].second);
        } else {
            auto n = returnTypeName(decl, slot);
            if (inHeader) {
                write("struct ", n, " {");
                bool first = true;
                for (auto& e : type->returnValues) {
                    if (first)
                        first = false;
                    else
                        write(", ");
                    writeType(e.second);
                    write(" ", e.first);
                };
                write("};");
            }
            write("[[nodiscard]] ", n);
        }
        write(" ", decl.getName(), "(");
        bool first = true, hasForward = false;
        for (auto& p : type->parameter) {
            if (first)
                first = false;
            else
                write(", ");
            using Direction = FunctionType::ParameterDirection;
            switch (p.direction) {
                case Direction::In:
                    write("cpp2::in<");
                    writeType(p.type);
                    write(">");
                    break;
                case Direction::Out:
                    write("cpp2::out<");
                    writeType(p.type);
                    write(">");
                    break;
                case Direction::Inout:
                    writeType(p.type);
                    write("&");
                    break;
                case Direction::Move:
                    writeType(p.type);
                    write("&&");
                    break;
                case Direction::Forward:
                    hasForward = true;
                    write("auto&&");
                    break;
                case Direction::Copy:
                    writeType(p.type);
                    break;
            }
            write(" ", p.name);
        }
        write(")");
        if (inHeader) {
            write(";\n");
        } else {
            if (hasForward) {
                write("\n requires ");
                first = true;
                for (auto& p : type->parameter) {
                    if (p.direction == FunctionType::ParameterDirection::Forward) {
                        if (first)
                            first = false;
                        else
                            write(" && ");
                        write("std::is_same_v<CPP2_TYPEOF(", p.name, "), ");
                        writeType(p.type);
                        write(")");
                    }
                }
                write("\n");
            }
            generateStatement(*o.statement);
        }
    } else if (inHeader) {
        // TODO
        write("??? ");
        write(decl.getName());
        write(";\n");
    }
}
//---------------------------------------------------------------------------
void CppOut::generateStatement(const Statement& s)
// Generate code for a statement
{
    switch (s.getType()) {
        case Statement::Type::Compound: {
            auto& c = static_cast<const CompoundStatement&>(s);
            advance(c.getBegin());
            write("{");

            for (auto& s : c.getStatements())
                generateStatement(*s);

            auto l = c.getEnd();
            if (l.column) --l.column;
            advance(l);
            write("}");
            break;
        }
        case Statement::Type::Return: {
            auto& r = static_cast<const ReturnStatement&>(s);
            advance(r.getBegin());
            write("return");
            if (r.getExpression()) {
                write(" ");
                generateExpression(*r.getExpression());
            }
            write(";");
            break;
        }
    }
}
//---------------------------------------------------------------------------
void CppOut::generateExpression(const Expression& e)
// Generate code for an expression
{
    using Category = Expression::Category;
    switch (e.getCategory()) {
        case Category::Literal: write(static_cast<const Literal&>(e).getText()); break;
    }
}
//---------------------------------------------------------------------------
void CppOut::generate(const Program& prog)
// Generate the C++1 code
{
    // Write the header first
    inBody = false;
    write("#include \"cpp2util.h\"\n");
    for (auto& d : prog.getSourceOrder()) {
        generateDeclaration(*d.first, d.second, true);
    }
    write("\n// Cpp2 definitions ---------------------------------------------------------\n\n");

    // We output the body now, reset the position information
    inBody = true;
    currentPos.line = 1;
    currentPos.column = 1;

    // We reconstruct the original source code order here
    for (auto& d : prog.getSourceOrder()) {
        generateDeclaration(*d.first, d.second, false);
    }
    write("\n");
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
