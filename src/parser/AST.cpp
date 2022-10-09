#include "AST.hpp"
#include "Lexer.hpp"
#include <iostream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
ASTContainer::~ASTContainer()
// Destructor
{
    while (chunks) {
        auto c = chunks;
        chunks = c->next;
        delete[] reinterpret_cast<std::byte*>(c);
    }
}
//---------------------------------------------------------------------------
void* ASTContainer::allocateRaw(unsigned requiredSize)
// Allocate memory for a node
{
    // Check if we need more space
    if ((currentEnd - currentBegin) < requiredSize) {
        unsigned newSize = std::max<uint64_t>(chunkSize, requiredSize);
        chunkSize += std::max<uint64_t>(chunkSize / 4, 1024);
        Chunk* c = new (new std::byte[sizeof(Chunk) + newSize]) Chunk();
        c->next = chunks;
        chunks = c;
        currentBegin = c->data;
        currentEnd = currentBegin + newSize;
    }

    auto result = currentBegin;
    currentBegin += requiredSize;
    return result;
}
//---------------------------------------------------------------------------
const AST* AST::dynCastImpl(Type requiredType, const AST* ast)
// Dyncast implementation. Checks that the type is correct
{
    // Make sure our AST node has the correct type
    if ((!ast) || (ast->type != requiredType)) {
        // Debug output
        std::cerr << "internal error, AST type confusion. Got ";
        if (!ast)
            std::cerr << "nullptr";
        else
            std::cerr << static_cast<unsigned>(ast->type);
        std::cerr << ", expected " << static_cast<unsigned>(requiredType) << std::endl;
        std::exit(1);
    }

    return ast;
}
//---------------------------------------------------------------------------
namespace ast {
//---------------------------------------------------------------------------
Token::Token(std::string_view content, unsigned fromLine, unsigned fromColumn, unsigned toLine, unsigned toColumn)
    : content(content), fromLine(fromLine), fromColumn(fromColumn), toLine(toLine), toColumn(toColumn) {
}
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
