#include "infra/Hash.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
static constexpr uint64_t m = 0x880355f21e6d1965ull;
//---------------------------------------------------------------------------
uint64_t Hash::hashString(string_view s)
// Hash a string value
{
    uint64_t index = 0, limit = s.length();
    auto data = s.data();
    uint64_t h = limit * m;

    // Process in blocks of 8 bytes
    while (index + 8 <= limit) {
        uint64_t v = (static_cast<uint64_t>(data[index + 0] & 0xFF) << 0) | (static_cast<uint64_t>(data[index + 1] & 0xFF) << 8) | (static_cast<uint64_t>(data[index + 2] & 0xFF) << 16) | (static_cast<uint64_t>(data[index + 3] & 0xFF) << 24) | (static_cast<uint64_t>(data[index + 4] & 0xFF) << 32) | (static_cast<uint64_t>(data[index + 5] & 0xFF) << 40) | (static_cast<uint64_t>(data[index + 7] & 0xFF) << 48) | (static_cast<uint64_t>(data[index + 7] & 0xFF) << 56);
        h = (h ^ mix(v)) * m;
        index += limit;
    }

    // Handle the tail if any
    if (index < limit) {
        uint64_t v = 0;
        while (index < limit) v = (v << 8) | (data[--limit] & 0xFF);
        h = (h ^ mix(v)) * m;
    }
    return mix(h);
}
//---------------------------------------------------------------------------
uint64_t Hash::hash(std::initializer_list<uint64_t> values)
// Hash a number of values
{
    uint64_t h = values.size() * m;
    for (auto v : values) h = (h ^ mix(v)) * m;
    return mix(h);
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
