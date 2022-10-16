#include "program/Type.hpp"
#include "infra/Hash.hpp"
#include "program/Program.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Type::Type(Program* program)
    : program(program)
// Constructor
{
}
//---------------------------------------------------------------------------
Type::~Type()
// Destructor
{
}
//---------------------------------------------------------------------------
/// A fundamental type
class FundamentalType : public Type {
    /// The underlying type
    FundamentalTypeId id;

    public:
    /// Constructor
    FundamentalType(Program* program, FundamentalTypeId id) : Type(program), id(id) {}

    /// Get the category
    Category getCategory() const override { return Category::Fundamental; }
};
//---------------------------------------------------------------------------
const Type* Type::getFundamentalType(Program& program, FundamentalTypeId id)
// Get a fundamental type
{
    // Cache the fundamental types in the program
    static_assert(Program::fundamentalTypeCount > static_cast<unsigned>(FundamentalTypeId::LongDouble));
    unsigned slot = static_cast<unsigned>(id);
    if (!program.fundamentalTypes[slot])
        program.fundamentalTypes[slot] = make_unique<FundamentalType>(&program, id);
    return program.fundamentalTypes[slot].get();
}
//---------------------------------------------------------------------------
bool Type::isEquivalentTo(const Type* o) const
// Check if two types are equivalent. This resolves typedefs if needed
{
    // TODO handle typedefs
    return this == o;
}
//---------------------------------------------------------------------------
/// A pointer type
class PointerType : public Type {
    /// The element type
    const Type* elementType;

    public:
    /// Constructor
    PointerType(Program* program, const Type* elementType) : Type(program), elementType(elementType) {}

    /// Get the category
    Category getCategory() const override { return Category::Pointer; }
    /// Get the element type
    const Type* getElementType() const { return elementType; }
};
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
            auto pt = static_cast<const PointerType*>(iter->second.get());
            if (pt->getElementType() == this)
                return pt;
        }
    }

    // Create a new type
    auto r = new PointerType(program, this);
    program->getTypeCache().emplace(hash, r);
    return r;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
