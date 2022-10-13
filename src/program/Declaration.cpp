#include "program/Declaration.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
Declaration::Declaration(string name, bool isFunction)
    : name(move(name)), func(isFunction) {
}
//---------------------------------------------------------------------------
Declaration::~Declaration() {
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
