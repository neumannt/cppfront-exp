#include "stdlib/Stdlib.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
// Note: In an ideal world this file would not exists. Ideally, we simply
// want to expose the existing C++1 standard library as-is in C++2. Doing so
// would be possible using, e.g., libclang.
// But using that would bring a very large dependency and a lot of complexity
// to this experiment. Thus, we instead mock the existing C++1 library in
// C++2 for now.
//---------------------------------------------------------------------------
const char* Stdlib::interface = R"stdlib(

namespace std {

using nullpt_t = decltype(nullptr);

}

)stdlib";
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
