#ifndef H_Program
#define H_Program
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <memory>
#include <unordered_map>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Type;
//---------------------------------------------------------------------------
/// Container for all program data
class Program {
    private:
    /// The type cache
    std::unordered_multimap<uint64_t, std::unique_ptr<Type>> typeCache;

    public:
    /// Constructor
    Program();
    /// Destructor
    ~Program();

    /// Access the type cache. Should only be used by type classes for interning
    auto& getTypeCache() { return typeCache; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
