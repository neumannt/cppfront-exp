#ifndef H_Hash
#define H_Hash
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include <initializer_list>
#include <string_view>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// Helper functions for hashing
class Hash {
    private:
    /// Mix logic. Constants taken from fast-hash
    static uint64_t mix(uint64_t h) {
        h ^= h >> 23;
        h *= 0x2127599bf4325c37ull;
        h ^= h >> 47;
        return h;
    }

    public:
    /// Hash a single value
    static uint64_t hashInt(uint64_t v) { return mix(v); }
    /// Hash a pointer value
    static uint64_t hashPtr(const void* v) { return mix(reinterpret_cast<uintptr_t>(v)); }
    /// Hash a string
    static uint64_t hashString(std::string_view s);
    /// Hash a sequence of values
    static uint64_t hash(std::initializer_list<uint64_t> values);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
