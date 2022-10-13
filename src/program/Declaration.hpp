#ifndef H_Declaration
#define H_Declaration
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <memory>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// A declaration within a namespace
class Declaration {
    private:
    /// The name
    std::string name;
    /// Is a function?
    bool func;

    public:
    /// Constructor
    Declaration(std::string name, bool isFunction);
    /// Destructor
    ~Declaration();

    /// Does this declaration describe a function?
    bool isFunction() const { return func; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
