#ifndef H_Range
#define H_Range
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include <string>
#include <string_view>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// An input range
struct Range {
    /// The boundaries
    std::size_t from, to;
};
//---------------------------------------------------------------------------
/// A source location
struct SourceLocation {
    /// The file
    std::string_view file;
    /// The position
    unsigned line, column;
};
//---------------------------------------------------------------------------
/// Logic for mapping ranges into source locations
class RangeMapping {
    /// The file name
    std::string_view fileName;
    /// The content of the file
    std::string_view content;

    /// The cached information
    unsigned cachedPos = 0, cachedLine = 1, cachedColumn = 1;

    public:
    /// Constructor
    RangeMapping(std::string_view fileName, std::string_view content) : fileName(fileName), content(content) {}

    /// Get the underlying content
    std::string_view getContent() const { return content; }

    /// Map a position
    SourceLocation mapPosition(unsigned pos);
    /// Get the begin of a range
    SourceLocation getBegin(Range range) { return mapPosition(range.from); }
    /// Get the end of a range
    SourceLocation getEnd(Range range) { return mapPosition(range.to); }
    /// Map a range
    std::pair<SourceLocation, SourceLocation> mapRang(Range range);
};
//---------------------------------------------------------------------------
/// An error description
struct Error {
    /// The occurrence in the source
    SourceLocation loc;
    /// The text
    std::string text;
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
