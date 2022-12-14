#ifndef H_FunctionType
#define H_FunctionType
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "program/Type.hpp"
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Program;
class Type;
//---------------------------------------------------------------------------
/// A function type
class FunctionType : public Type {
    public:
    /// Flags that can be attached to a type
    enum TypeFlags {
        ReturnsRef,
        Throws
    };
    /// Flags that can be attached to a function declaration
    enum DeclarationFlags {
        Static,
        Virtual,
        Abstract,
        Override,
        Final,
        Defaulted,
        Deleted
    };
    /// Known directions
    enum class ParameterDirection {
        In,
        Out,
        Inout,
        Move,
        Copy,
        Forward
    };
    /// A parameter description
    struct Parameter {
        /// The name
        std::string name;
        /// The type
        const Type* type;
        /// The direction
        ParameterDirection direction = ParameterDirection::In;

        /// Enable comparisons
        auto operator<=>(const Parameter& p) const = default;
    };
    /// The parameters
    std::vector<Parameter> parameter;
    /// The return values
    std::vector<std::pair<std::string, const Type*>> returnValues;
    /// Type flags (bitmask)
    unsigned typeFlags;

    private:
    /// Constructor
    FunctionType(Program* program, std::vector<Parameter>&& parameter, std::vector<std::pair<std::string, const Type*>>&& returnValues, unsigned typeFlags);

    public:
    /// Destructor
    ~FunctionType();

    /// Get the type category
    Category getCategory() const override { return Category::Function; }
    /// Access the parameters
    auto& getParameters() const { return parameter; }
    /// Get the return values
    auto& getReturnValues() const { return returnValues; }
    /// Check for flags
    bool hasFlag(TypeFlags f) const { return typeFlags & (1 << f); }

    /// Create or lookup a function type
    static const FunctionType* get(Program& prog, std::vector<Parameter>&& parameter, std::vector<std::pair<std::string, const Type*>>&& returnValues, unsigned typeFlags);
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
