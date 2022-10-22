#ifndef H_Declaration
#define H_Declaration
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Expression;
class FunctionType;
class Namespace;
class Statement;
//---------------------------------------------------------------------------
/// A function id
struct FunctionId {
    /// Known categories
    enum Category {
        Regular,
        OperatorAnd,
        OperatorOr,
        OperatorBitAnd,
        OperatorBitOr,
        OperatorBitXor,
        OperatorEqual,
        OperatorNotEqual,
        OperatorLess,
        OperatorLessEq,
        OperatorGreater,
        OperatorGreaterEq,
        OperatorSpaceship,
        OperatorLeftShift,
        OperatorRightShift,
        OperatorPlus,
        OperatorMinus,
        OperatorMul,
        OperatorDiv,
        OperatorModulo
    };

    /// The name (if any)
    std::string name;
    /// The category
    Category category;

    /// Construct a regular function
    FunctionId(std::string name) : name(std::move(name)), category(Regular) {}
    /// Construct a special operator function
    FunctionId(Category category) : category(category) {}
};
//---------------------------------------------------------------------------
/// A declaration within a namespace
class Declaration {
    public:
    /// The declaration category
    enum class Category {
        Variable,
        Function,
        Namespace
    };

    private:
    /// The source location (for pretty printing)
    SourceLocation loc;
    /// The name
    std::string name;

    public:
    /// Constructor
    Declaration(SourceLocation loc, std::string name);
    /// Destructor
    ~Declaration();

    /// Get the location
    auto& getLocation() const { return loc; }
    /// Get the name
    std::string_view getName() const { return name; }

    /// Get the declaration category
    virtual Category getCategory() const = 0;
    /// Does this declaration describe a function?
    bool isFunction() const { return getCategory() == Category::Function; }
};
//---------------------------------------------------------------------------
/// A variable declaration
class VariableDeclaration : public Declaration {
    public:
    /// Constructor
    VariableDeclaration(SourceLocation loc, std::string name);
    /// Destructor
    ~VariableDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Variable; };
};
//---------------------------------------------------------------------------
/// A function declaration
class FunctionDeclaration : public Declaration {
    public:
    /// An overload entry
    struct Overload {
        /// The source location (for pretty printing)
        SourceLocation loc;
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
    /// The overloads (if a function)
    std::vector<Overload> overloads;

    public:
    /// Constructor
    FunctionDeclaration(SourceLocation loc, std::string name);
    /// Destructor
    ~FunctionDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Function; };

    /// Check if an overload exists. This ignores parameter names and return types
    Overload* findFunctionOverload(const FunctionType* type);
    /// Add a new function overload
    unsigned addFunctionOverload(SourceLocation loc, const FunctionType* type, std::vector<std::unique_ptr<Expression>>&& defaultArguments, unsigned defaultArgumentsOffset);
    /// Get the number of overload entries
    unsigned getOverloadCount() const { return overloads.size(); }
    /// Access a certain overload
    Overload& accessOverload(unsigned slot) { return overloads[slot]; }
    /// Access a certain overload
    const Overload& accessOverload(unsigned slot) const { return overloads[slot]; }
};
//---------------------------------------------------------------------------
/// A namespace declaration
class NamespaceDeclaration : public Declaration {
    /// The underlying namespace
    std::unique_ptr<Namespace> ns;

    public:
    /// Constructor
    NamespaceDeclaration(SourceLocation loc, std::string name);
    /// Destructor
    ~NamespaceDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Namespace; };

    /// Get the contained namespace
    Namespace* getNamespace() { return ns.get(); }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
