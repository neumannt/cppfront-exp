#ifndef H_Type
#define H_Type
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include <memory>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Program;
class Type;
//---------------------------------------------------------------------------
/// Base class for all types. Types are interned, i.e., if two types are
/// identical the pointers are identical
class Type {
    public:
    /// Known type categories
    enum class Category {
        Fundamental,
        Function,
        Pointer
    };
    /// Fundamental types
    enum class FundamentalTypeId {
        Void,
        Char,
        UnsignedChar,
        Char8,
        Char16,
        Char32,
        WChar,
        Short,
        UnsignedShort,
        Int,
        UnsignedInt,
        Long,
        UnsignedLong,
        LongLong,
        UnsignedLongLong,
        Bool,
        Float,
        Double,
        LongDouble,
        NullptrType
    };

    /// Integer type?
    static bool isInteger(FundamentalTypeId id) { return (id >= FundamentalTypeId::Int) && (id <= FundamentalTypeId::UnsignedLongLong); }
    /// Unsigned integer?
    static bool isUnsignedInt(FundamentalTypeId id) { return (id == FundamentalTypeId::UnsignedInt) || (id == FundamentalTypeId::UnsignedLong) || (id == FundamentalTypeId::UnsignedLongLong); }
    /// Floating point data type?
    static bool isFloatingPoint(FundamentalTypeId id) { return (id >= FundamentalTypeId::Float) && (id <= FundamentalTypeId::LongDouble); }
    /// Numerical data type
    static bool isNumerical(FundamentalTypeId id) { return ((id >= FundamentalTypeId::Char) && (id <= FundamentalTypeId::UnsignedLongLong)) || isFloatingPoint(id); }

    protected:
    /// The containing program
    Program* program;
    /// The effective type
    const Type* effectiveType;

    /// Constructor
    explicit Type(Program* program);

    public:
    /// Destructor
    virtual ~Type();

    /// Get the containing program
    Program* getProgram() const { return program; }
    /// Get the type category
    virtual Category getCategory() const = 0;
    /// Is a typedef?
    virtual bool isTypedef() const { return false; }
    /// Is a function type?
    bool isFunctionType() const { return getCategory() == Category::Function; }
    /// Is a function type?
    bool isFundamentalType() const { return getCategory() == Category::Fundamental; }
    /// Is a function type?
    bool isPointerType() const { return getCategory() == Category::Pointer; }

    /// Get the effective type. Resolves typedefs if needed
    const Type* getEffectiveType() const { return effectiveType; }
    /// Check if two types are equivalent. This resolves typedefs if needed
    bool isEquivalentTo(const Type* o) const;
    /// Get a pointer to the current type
    const Type* getPointerTo() const;
    /// Cast as a subclass
    template <class T>
    const T* as() const { return static_cast<const T*>(effectiveType); }

    /// Get a fundamental type
    static const Type* getFundamentalType(Program& program, FundamentalTypeId id);
    /// Get a type representation for 'void'
    static const Type* getVoid(Program& program) { return getFundamentalType(program, FundamentalTypeId::Void); }
    /// Get a type representation for 'char'
    static const Type* getChar(Program& program) { return getFundamentalType(program, FundamentalTypeId::Char); }
    /// Get a type representation for 'unsigned char'
    static const Type* getUnsignedChar(Program& program) { return getFundamentalType(program, FundamentalTypeId::UnsignedChar); }
    /// Get a type representation for 'char8_t'
    static const Type* getChar8(Program& program) { return getFundamentalType(program, FundamentalTypeId::Char8); }
    /// Get a type representation for 'char16_t'
    static const Type* getChar16(Program& program) { return getFundamentalType(program, FundamentalTypeId::Char16); }
    /// Get a type representation for 'char32_t'
    static const Type* getChar32(Program& program) { return getFundamentalType(program, FundamentalTypeId::Char32); }
    /// Get a type representation for 'wchar_t'
    static const Type* getWChar(Program& program) { return getFundamentalType(program, FundamentalTypeId::WChar); }
    /// Get a type representation for 'int'
    static const Type* getInt(Program& program) { return getFundamentalType(program, FundamentalTypeId::Int); }
    /// Get a type representation for 'unsigned'
    static const Type* getUnsignedInt(Program& program) { return getFundamentalType(program, FundamentalTypeId::UnsignedInt); }
    /// Get a type representation for 'short'
    static const Type* getShort(Program& program) { return getFundamentalType(program, FundamentalTypeId::Short); }
    /// Get a type representation for 'unsigned short'
    static const Type* getUnsignedShort(Program& program) { return getFundamentalType(program, FundamentalTypeId::UnsignedShort); }
    /// Get a type representation for 'long'
    static const Type* getLong(Program& program) { return getFundamentalType(program, FundamentalTypeId::Long); }
    /// Get a type representation for 'unsigned long'
    static const Type* getUnsignedLong(Program& program) { return getFundamentalType(program, FundamentalTypeId::UnsignedLong); }
    /// Get a type representation for 'long long'
    static const Type* getLongLong(Program& program) { return getFundamentalType(program, FundamentalTypeId::LongLong); }
    /// Get a type representation for 'unsigned long long'
    static const Type* getUnsignedLongLong(Program& program) { return getFundamentalType(program, FundamentalTypeId::UnsignedLongLong); }
    /// Get a type representation for 'bool'
    static const Type* getBool(Program& program) { return getFundamentalType(program, FundamentalTypeId::Bool); }
    /// Get a type representation for 'float'
    static const Type* getFloat(Program& program) { return getFundamentalType(program, FundamentalTypeId::Float); }
    /// Get a type representation for 'double'
    static const Type* getDouble(Program& program) { return getFundamentalType(program, FundamentalTypeId::Double); }
    /// Get a type representation for 'long double'
    static const Type* getLongDouble(Program& program) { return getFundamentalType(program, FundamentalTypeId::LongDouble); }
    /// Get a type representation for 'nullptr_t'
    static const Type* getNullptrType(Program& program) { return getFundamentalType(program, FundamentalTypeId::NullptrType); }
};
//---------------------------------------------------------------------------
/// A fundamental type
class FundamentalType : public Type {
    /// The underlying type
    FundamentalTypeId id;

    public:
    /// Constructor
    FundamentalType(Program* program, FundamentalTypeId id) : Type(program), id(id) {}

    /// Get the category
    Category getCategory() const override { return Category::Fundamental; }
    /// Get the underlying type
    FundamentalTypeId getId() const { return id; }
};
//---------------------------------------------------------------------------
/// A pointer type
class PointerType : public Type {
    /// The element type
    const Type* elementType;

    public:
    /// Constructor
    PointerType(Program* program, const Type* elementType, const Type* et) : Type(program), elementType(elementType) {
        if (et) effectiveType = et;
    }

    /// Get the category
    Category getCategory() const override { return Category::Pointer; }
    /// Get the element type
    const Type* getElementType() const { return elementType; }
};
//---------------------------------------------------------------------------
/// An alias type
class AliasType : public Type {
    public:
    /// Constructor
    AliasType(Program* program, const Type* originalType) : Type(program) { effectiveType = originalType->getEffectiveType(); }

    /// Get the category
    Category getCategory() const override { return effectiveType->getCategory(); }
    /// Is a typedef?
    bool isTypedef() const override { return true; }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
