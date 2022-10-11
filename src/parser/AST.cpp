#include "AST.hpp"
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
}
//---------------------------------------------------------------------------
