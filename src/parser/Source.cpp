#include "Source.hpp"
#include <iostream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include <string_view>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
AST* Source::astImpl(AST::Type type, unsigned subType, Range range, std::initializer_list<const AST*> args, unsigned flags)
// Create an AST node
{
    auto node = new (container.allocateRaw(sizeof(AST) + (sizeof(void*) * args.size()))) AST(type, flags, args.size(), subType, range);
    unsigned slot = 0;
    for (auto a : args) node->children[slot++] = a;
    return node;
}
//---------------------------------------------------------------------------
int Source::reportUnknownError()
// Report an unknown error
{
    RangeMapping mapping(fileName, content);
    auto sourcePos = mapping.mapPosition(pos ? pos - 1 : 0);
    std::cerr << sourcePos.file << ":" << sourcePos.line << ":" << sourcePos.column << ":syntax error" << std::endl;
    return -1;
}
//---------------------------------------------------------------------------
int Source::reportError(const char* message, Range pos)
// Report an error
{
    RangeMapping mapping(fileName, content);
    auto sourcePos = mapping.getBegin(pos);
    std::cerr << sourcePos.file << ":" << sourcePos.line << ":" << sourcePos.column << ":" << message << std::endl;
    return -1;
}
//---------------------------------------------------------------------------
AST* Source::ast(AST::Type type, Range range) { return astImpl(type, 0, range, {}); }
AST* Source::ast2(AST::Type type, unsigned subType, Range range) { return astImpl(type, subType, range, {}); }
AST* Source::ast(AST::Type type, Range range, const AST* a1) { return astImpl(type, 0, range, {a1}); }
AST* Source::ast2(AST::Type type, unsigned subType, Range range, const AST* a1) { return astImpl(type, subType, range, {a1}); }
AST* Source::ast(AST::Type type, Range range, const AST* a1, const AST* a2) { return astImpl(type, 0, range, {a1, a2}); }
AST* Source::ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2) { return astImpl(type, subType, range, {a1, a2}); }
AST* Source::ast(AST::Type type, Range range, const AST* a1, const AST* a2, const AST* a3) { return astImpl(type, 0, range, {a1, a2, a3}); }
AST* Source::ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2, const AST* a3) { return astImpl(type, subType, range, {a1, a2, a3}); }
AST* Source::ast(AST::Type type, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4) { return astImpl(type, 0, range, {a1, a2, a3, a4}); }
AST* Source::ast2(AST::Type type, unsigned subType, Range range, const AST* a1, const AST* a2, const AST* a3, const AST* a4) { return astImpl(type, subType, range, {a1, a2, a3, a4}); }
//---------------------------------------------------------------------------
AST* Source::buildList(AST::Type type, Range range, const AST* element)
// Build a list with a single element
{
    return astImpl(type, 0, range, {element, nullptr, nullptr}, 1);
}
//---------------------------------------------------------------------------
AST* Source::appendList(AST::Type type, Range range, AST* list, const AST* element)
// Append to a list
{
    auto tail = buildList(type, range, element);
    if (list->children[2])
        const_cast<AST*>(list->children[2])->children[1] = tail;
    else
        list->children[1] = tail;
    list->children[2] = tail;
    return list;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#if 0
int main(int argc, char* argv[]) {
   if (argc==2) {
      cpp2exp::Source s(argv[1]);
      cpp2parser_context_t *ctx = cpp2parser_create(&s);
      cpp2exp::AST* tree = nullptr;
      auto res = cpp2parser_parse(ctx, &tree);
      std::cerr << "result: " << res << std::endl;
      cpp2parser_destroy(ctx);
   }
}
#endif
