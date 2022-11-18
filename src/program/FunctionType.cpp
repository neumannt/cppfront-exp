#include "program/FunctionType.hpp"
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
FunctionType::FunctionType(Program* program, vector<Parameter>&& parameter, vector<pair<string, const Type*>>&& returnValues, unsigned typeFlags)
    : Type(program), parameter(std::move(parameter)), returnValues(std::move(returnValues)), typeFlags(typeFlags) {
}
//---------------------------------------------------------------------------
FunctionType::~FunctionType() {
}
//---------------------------------------------------------------------------
const FunctionType* FunctionType::get(Program& prog, vector<Parameter>&& parameter, vector<pair<string, const Type*>>&& returnValues, unsigned typeFlags)
// Create or lookup a function type
{
    // Compute a hash value of the signature
    uint64_t hash = Hash::hash({static_cast<unsigned>(Type::Category::Function), parameter.size(), returnValues.size(), typeFlags});
    for (auto& p : parameter)
        hash = Hash::hash({hash, Hash::hashString(p.name), Hash::hashPtr(p.type), static_cast<unsigned>(p.direction)});
    for (auto& r : returnValues)
        hash = Hash::hash({hash, Hash::hashString(r.first), Hash::hashPtr(r.second)});

    // Check if the type already exists
    auto range = prog.getTypeCache().equal_range(hash);
    for (auto iter = range.first; iter != range.second; ++iter) {
        if (iter->second->isFunctionType()) {
            auto ft = iter->second->as<FunctionType>();
            if ((ft->parameter == parameter) && (ft->returnValues == returnValues) && (ft->typeFlags == typeFlags))
                return ft;
        }
    }

    // Create a new type
    auto r = new FunctionType(&prog, std::move(parameter), std::move(returnValues), typeFlags);
    prog.getTypeCache().emplace(hash, r);
    return r;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
