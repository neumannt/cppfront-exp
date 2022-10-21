#include "AST.hpp"
#include <cassert>
#include <iostream>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
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
const AST* AST::get(unsigned element, [[maybe_unused]] Type nodeType) const
// Access an element and make sure that the node type matches. Accessing a null pointer is an error
{
    assert(element < childCount);
    auto r = children[element];
    assert(r && r->getType() == nodeType);
    return r;
}
//---------------------------------------------------------------------------
const AST* AST::getOrNull(unsigned element, [[maybe_unused]] Type nodeType) const
// Access an element and make sure that the node type matches. A null pointer is valid
{
    assert(element < childCount);
    auto r = children[element];
    assert((!r) || r->getType() == nodeType);
    return r;
}
//---------------------------------------------------------------------------
const AST* AST::getAny(unsigned element) const
// Access an element with enforcing a specific node type
{
    assert(element < childCount);
    auto r = children[element];
    assert(r);
    return r;
}
//---------------------------------------------------------------------------
const AST* AST::getAnyOrNull(unsigned element) const
// Access an element with enforcing a specific node type
{
    assert(element < childCount);
    return children[element];
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
