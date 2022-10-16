#ifndef H_Declaration
#define H_Declaration
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Expression;
class FunctionType;
class Statement;
//---------------------------------------------------------------------------
/// A declaration within a namespace
class Declaration {
    public:
    /// An overload entry
    struct Overload {
        /// The function type of that overload
        const FunctionType* type;
        /// The default values (if any)
        std::vector<std::unique_ptr<Expression>> defaultArguments;
        /// The start of the default values
        unsigned defaultArgumentsOffset;
        /// The body
        std::unique_ptr<Statement> statement;
    };

    private:
    /// The name
    std::string name;
    /// The overloads (if a function)
    std::vector<Overload> overloads;
    /// Is a function?
    bool func;

    public:
    /// Constructor
    Declaration(std::string name, bool isFunction);
    /// Destructor
    ~Declaration();

    /// Does this declaration describe a function?
    bool isFunction() const { return func; }
    /// Check if an overload exists. This ignores parameter names and return types
    Overload* findFunctionOverload(const FunctionType* type);
    /// Add a new function overload
    Overload* addFunctionOverload(const FunctionType* type, std::vector<std::unique_ptr<Expression>>&& defaultArguments, unsigned defaultArgumentsOffset);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
