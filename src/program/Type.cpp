#include "program/Type.hpp"
#include "infra/Hash.hpp"
#include "program/Program.hpp"
#include <cassert>
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
const Type* Type::getUnderlyingType() const
// Get the underlying type. Resolves typdefs and strips const
{
    auto t = getEffectiveType();
    if (t->isConst()) t = static_cast<const ConstType*>(t)->getBaseType();
    return t;
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
        if (iter->second->isPointerType()) {
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
enum class TypeTags {
    Const = static_cast<unsigned>(Type::Category::Class) + 1
};
//---------------------------------------------------------------------------
const Type* Type::getAsConst() const
// Get as const type
{
    if (isConst()) return this;

    // Compute a hash value of the signature
    uint64_t hash = Hash::hash({static_cast<unsigned>(TypeTags::Const), Hash::hashPtr(this)});

    // Check if the type already exists
    auto range = program->getTypeCache().equal_range(hash);
    for (auto iter = range.first; iter != range.second; ++iter) {
        if (iter->second->isConst()) {
            auto ct = static_cast<const ConstType*>(iter->second.get());
            if (ct->getBaseType() == this)
                return ct;
        }
    }

    // Handle the effective type if needed
    const Type* et = nullptr;
    if (this != getEffectiveType())
        et = getEffectiveType()->getAsConst();

    // Create a new type
    auto r = new ConstType(program, this, et);
    program->getTypeCache().emplace(hash, r);
    return r;
}
//---------------------------------------------------------------------------
const Type* Type::getAsNonConst() const
// Get as non-const type
{
    if (!isConst()) return this;

    return static_cast<const ConstType*>(this)->getBaseType();
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
