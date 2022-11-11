#ifndef H_Declaration
#define H_Declaration
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
#include <functional>
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Class;
class Expression;
class FunctionType;
class Namespace;
class Program;
class Statement;
class Type;
//---------------------------------------------------------------------------
/// A function id
struct DeclarationId {
    /// Known categories
    enum Category {
        Regular,
        OperatorAnd,
        OperatorAssignment,
        OperatorBitAnd,
        OperatorBitAndEq,
        OperatorBitOr,
        OperatorBitOrEq,
        OperatorBitXor,
        OperatorBitXorEq,
        OperatorComplement,
        OperatorDiv,
        OperatorDivEq,
        OperatorEqual,
        OperatorGreater,
        OperatorGreaterEq,
        OperatorLeftShift,
        OperatorLeftShiftEq,
        OperatorLess,
        OperatorLessEq,
        OperatorMinus,
        OperatorMinusEq,
        OperatorMinusMinus,
        OperatorModulo,
        OperatorModuloEq,
        OperatorMul,
        OperatorMulEq,
        OperatorNot,
        OperatorNotEqual,
        OperatorOr,
        OperatorPlus,
        OperatorPlusEq,
        OperatorPlusPlus,
        OperatorRightShift,
        OperatorRightShiftEq,
        OperatorSpaceship
    };

    /// The name (if any)
    std::string name;
    /// The category
    Category category;

    /// Construct a regular function
    DeclarationId(std::string name) : name(std::move(name)), category(Regular) {}
    /// Construct a regular function
    explicit DeclarationId(std::string_view name) : name(name), category(Regular) {}
    /// Construct a special operator function
    DeclarationId(Category category) : category(category) {}

    /// Is a regular id?
    bool isRegular() const { return category == Category::Regular; }

    auto operator<=>(const DeclarationId&) const = default;
};
//---------------------------------------------------------------------------
/// A declaration within a namespace
class Declaration {
    public:
    /// The declaration category
    enum class Category {
        Variable,
        Function,
        Namespace,
        Class,
        Typedef
    };

    private:
    /// The source location (for pretty printing)
    SourceLocation loc;
    /// The name
    DeclarationId name;
    /// The containing namespace (if any)
    Namespace* containingNamespace;

    public:
    /// Constructor
    Declaration(SourceLocation loc, DeclarationId name, Namespace* containingNamespace);
    /// Destructor
    virtual ~Declaration();

    /// Get the location
    auto& getLocation() const { return loc; }
    /// Get the name
    auto& getName() const { return name; }
    /// Get the containing namespace
    auto getContainingNamespace() const { return containingNamespace; }

    /// Get the declaration category
    virtual Category getCategory() const = 0;
    /// Does this declaration describe a function?
    bool isFunction() const { return getCategory() == Category::Function; }
    /// The this declaration describe a type?
    bool isType() const { return getCategory() == Category::Class || getCategory() == Category::Typedef; }
    /// Get the corresponding type (if any)
    virtual const Type* getCorrespondingType() const;
};
//---------------------------------------------------------------------------
/// A variable declaration
class VariableDeclaration : public Declaration {
    /// The data type
    const Type* type;

    public:
    /// Constructor
    VariableDeclaration(SourceLocation loc, DeclarationId name, Namespace* containingNamespace, const Type* type);
    /// Destructor
    ~VariableDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Variable; };
    /// Get the type
    const Type* getType() const { return type; }
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
    FunctionDeclaration(SourceLocation loc, DeclarationId name, Namespace* containingNamespace);
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
    NamespaceDeclaration(SourceLocation loc, DeclarationId name, Namespace* parent);
    /// Destructor
    ~NamespaceDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Namespace; };

    /// Get the contained namespace
    Namespace* getNamespace() const { return ns.get(); }
};
//---------------------------------------------------------------------------
/// A class declaration
class ClassDeclaration : public Declaration {
    /// The underlying class
    std::unique_ptr<Class> cl;

    public:
    /// Constructor
    ClassDeclaration(SourceLocation loc, DeclarationId name, Namespace* parent, Program* program);
    /// Destructor
    ~ClassDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Class; };
    /// Get the corresponding type (if any)
    const Type* getCorrespondingType() const override;

    /// Get the contained class
    Class* getClass() const { return cl.get(); }
};
//---------------------------------------------------------------------------
/// A typedef declaration declaration
class TypedefDeclaration : public Declaration {
    /// The new type
    std::unique_ptr<Type> newType;

    public:
    /// Constructor
    TypedefDeclaration(SourceLocation loc, DeclarationId name, Namespace* containingNamespace, const Type* originalType);
    /// Destructor
    ~TypedefDeclaration();

    /// Get the declaration category
    Category getCategory() const override { return Category::Typedef; };

    /// Get the new type
    const Type* getType() const { return newType.get(); }
    /// Get the corresponding type (if any)
    const Type* getCorrespondingType() const override { return getType(); }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
/// Hasher
template <>
struct std::hash<cpp2exp::DeclarationId> {
    std::size_t operator()(const cpp2exp::DeclarationId& id) const noexcept;
};
//---------------------------------------------------------------------------
#endif
