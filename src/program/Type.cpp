#include "program/Type.hpp"
#include "infra/Hash.hpp"
#include "program/Program.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Type::Type(Program* program)
    : program(program), effectiveType(this)
// Constructor
{
}
//---------------------------------------------------------------------------
Type::~Type()
// Destructor
{
}
//---------------------------------------------------------------------------
const Type* Type::getFundamentalType(Program& program, FundamentalTypeId id)
// Get a fundamental type
{
    // Cache the fundamental types in the program
    static_assert(Program::fundamentalTypeCount > static_cast<unsigned>(FundamentalTypeId::NullptrType));
    unsigned slot = static_cast<unsigned>(id);
    if (!program.fundamentalTypes[slot])
        program.fundamentalTypes[slot] = make_unique<FundamentalType>(&program, id);
    return program.fundamentalTypes[slot].get();
}
//---------------------------------------------------------------------------
bool Type::isEquivalentTo(const Type* o) const
// Check if two types are equivalent. This resolves typedefs if needed
{
    return (o == this) || (getEffectiveType() == o->getEffectiveType());
}
//---------------------------------------------------------------------------
const Type* Type::getPointerTo() const
// Get a pointer to the current type
{
    // Compute a hash value of the signature
    uint64_t hash = Hash::hash({static_cast<unsigned>(Type::Category::Pointer), Hash::hashPtr(this)});

    // Check if the type already exists
    auto range = program->getTypeCache().equal_range(hash);
    for (auto iter = range.first; iter != range.second; ++iter) {
        if (iter->second->isFunctionType()) {
            auto pt = iter->second->as<PointerType>();
            if (pt->getElementType() == this)
                return pt;
        }
    }

    // Handle the effective type if needed
    const Type* et = nullptr;
    if (this != getEffectiveType())
        et = getEffectiveType()->getPointerTo();

    // Create a new type
    auto r = new PointerType(program, this, et);
    program->getTypeCache().emplace(hash, r);
    return r;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
