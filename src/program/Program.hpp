#ifndef H_Program
#define H_Program
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <memory>
#include <unordered_map>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Declaration;
class Type;
//---------------------------------------------------------------------------
/// Container for all program data
class Program {
    private:
    /// Maximum number of fundamental types
    static constexpr unsigned fundamentalTypeCount = 20;

    /// The type cache
    std::unordered_multimap<uint64_t, std::unique_ptr<Type>> typeCache;
    /// The cached fundamental types
    std::array<std::unique_ptr<Type>, fundamentalTypeCount> fundamentalTypes;
    /// The source order (for pretty printing)
    std::vector<std::pair<Declaration*, unsigned>> sourceOrder;

    friend class Type;

    public:
    /// Constructor
    Program();
    /// Destructor
    ~Program();

    /// Access the type cache. Should only be used by type classes for interning
    auto& getTypeCache() { return typeCache; }
    /// Keep track of the order of declarations
    void trackSourceOrder(Declaration* d, unsigned s) { sourceOrder.emplace_back(d, s); }
    /// Access the source order
    auto& getSourceOrder() const { return sourceOrder; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
