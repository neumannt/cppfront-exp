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
        Function,
        Pointer
    };

    protected:
    /// The containing program
    Program* program;

    /// Constructor
    explicit Type(Program* program);

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
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
