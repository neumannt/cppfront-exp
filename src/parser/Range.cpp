#include "Range.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
SourceLocation RangeMapping::mapPosition(unsigned pos)
// Map a position
{
    if (pos < cachedPos) {
        cachedPos = 0;
        cachedLine = 1;
        cachedColumn = 1;
    }
    if (pos > content.length()) pos = content.length();
    while (cachedPos < pos) {
        char c = content[cachedPos++];
        if (c == '\n') {
            if ((cachedPos < content.length()) && (content[cachedPos] == '\r')) ++cachedPos;
            cachedLine++;
            cachedColumn = 1;
        } else if (c == '\r') {
            cachedLine++;
            cachedColumn = 1;
        } else {
            cachedColumn++;
        }
    }
    return {fileName, cachedLine, cachedColumn};
}
//---------------------------------------------------------------------------
std::pair<SourceLocation, SourceLocation> RangeMapping::mapRang(Range range)
// Map a range
{
    auto from = mapPosition(range.from);
    auto to = mapPosition(range.to);
    return {from, to};
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
