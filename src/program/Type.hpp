#ifndef H_Type
#define H_Type
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
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
        LongDouble
    };

    protected:
    /// The containing program
    Program* program;

    /// Constructor
    explicit Type(Program* program);

    /// Get a fundamental type
    static const Type* getFundamentalType(Program& program, FundamentalTypeId id);

    public:
    /// Destructor
    virtual ~Type();

    /// Get the type category
    virtual Category getCategory() const = 0;
    /// Is a function type?
    bool isFunctionType() const { return getCategory() == Category::Function; }

    /// Check if two types are equivalent. This resolves typedefs if needed
    bool isEquivalentTo(const Type* o) const;
    /// Get a pointer to the current type
    const Type* getPointerTo() const;

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
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
