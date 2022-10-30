#ifndef H_Module
#define H_Module
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include <memory>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Namespace;
//---------------------------------------------------------------------------
/// A C++ module
class Module {
    private:
    /// The global namespace
    std::unique_ptr<Namespace> globalNamespace;

    public:
    /// Constructor
    Module();
    /// Destructor
    ~Module();

    /// Access the global namespace
    Namespace* getGlobalNamespace() const { return globalNamespace.get(); }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
