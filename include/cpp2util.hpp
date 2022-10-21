#ifndef H_cpp2util
#define H_cpp2util
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
// Simulate import std;
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2 {
//---------------------------------------------------------------------------
/// Calling convention for in parameters
template <typename T>
using in = std::conditional_t<sizeof(T) <= 2 * sizeof(void*) && std::is_trivially_copy_constructible_v<T>, const T, const T&>;
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
