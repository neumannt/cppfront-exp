#ifndef H_Stdlib
#define H_Stdlib
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// Interface to the existing C++1 stdlib
class Stdlib {
    public:
    /// Interface definitions
    static const char* interface;
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
