#ifndef H_AST
#define H_AST
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
#include <string>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
/// Base class for AST nodes
class AST {
    public:
    /// Known types
    enum class Type {
        Token,
        Declaration,
        DeclarationSeq,
        UnnamedDeclaration,
        TranslationUnit
    };

    private:
    /// The type
    Type type;

    /// Dyncast implementation. Checks that the type is correct
    static const AST* dynCastImpl(Type requiredType, const AST* ast);

    public:
    /// Constructor
    explicit AST(Type type) : type(type) {}

    /// Get the node type
    Type getType() const { return type; }
    /// Safely cast to a given type. Terminates if the type does not match.
    template <class T>
    static const T* dynCastHelper(Type type, const AST* ast) { return static_cast<const T*>(dynCastImpl(type, ast)); }
};
//---------------------------------------------------------------------------
/// Container for managing AST nodes. We use that instead of a simple unique_ptr to avoid stack overflow in deep trees
class ASTContainer {
    private:
    /// A chunk of memory
    struct Chunk {
        /// The next chunk
        Chunk* next;
        /// The data
        std::byte data[];
    };
    /// All chunks
    Chunk* chunks = nullptr;
    /// The current memory region
    std::byte *currentBegin = nullptr, *currentEnd = nullptr;
    /// The result tree
    const AST* result = nullptr;
    /// The allocation size for new chunks
    std::uint64_t chunkSize = 0;

    ASTContainer(const ASTContainer&) = delete;
    void operator=(const ASTContainer&) = delete;

    public:
    /// Constructor
    ASTContainer() = default;
    /// Destructor
    ~ASTContainer();

    /// Allocate memory for an AST node
    void* allocateRaw(unsigned size);
    /// Allocate memory for an AST node
    template <class T>
    void* allocate() { return allocateRaw(sizeof(T)); }

    /// Get the resulting parse tree
    const AST* getResult() const { return result; }
    /// Remember the parse tree
    void setResult(const AST* r) { result = r; }
};
//---------------------------------------------------------------------------
namespace ast {
//---------------------------------------------------------------------------
/// Helper to simplify node construction
template <class T, AST::Type typeId>
class ASTNode : public AST {
    public:
    /// Constructor
    ASTNode() : AST(typeId) {}

    /// Cast helper
    static const T* dynCast(const AST* ast) { return dynCastHelper<T>(typeId, ast); }
};
//---------------------------------------------------------------------------
/// A terminal token
class Token : public ASTNode<Token, AST::Type::Token> {
    public:
    /// The content
    std::string_view content;
    /// Position in text
    unsigned fromLine, fromColumn, toLine, toColumn;

    /// Constructor
    Token(std::string_view content, unsigned fromLine, unsigned fromColumn, unsigned toLine, unsigned toColumn);
};
//---------------------------------------------------------------------------
/// A declaration
class Declaration : public ASTNode<Declaration, AST::Type::Declaration> {
    public:
    /// The name
    const Token* name;
    /// The body
    const AST* body;

    /// Constructor
    Declaration(const Token* name, const AST* body) : name(name), body(body) {}

    /// Build
    static Declaration* build(ASTContainer& c, const AST* name, const AST* body) { return new (c.allocate<Declaration>()) Declaration(Token::dynCast(name), body); }
};
//---------------------------------------------------------------------------
/// An unnamed declaration
class UnnamedDeclaration : public ASTNode<Declaration, AST::Type::UnnamedDeclaration> {
    public:
    /// Subtypes
    enum SubType {
        Value,
        Function
    };
    /// The subtype
    SubType subType;
    /// The declared type
    const AST* declaredType;
    /// The value
    const AST* value;

    /// Constructor
    UnnamedDeclaration(SubType subType, const AST* declaredType, const AST* value) : subType(subType), declaredType(declaredType), value(value) {}

    /// Build
    static UnnamedDeclaration* build(ASTContainer& c, SubType subType, const AST* declaredType, const AST* value) { return new (c.allocate<UnnamedDeclaration>()) UnnamedDeclaration(subType, declaredType, value); }
};
//---------------------------------------------------------------------------
/// A list of declarations
class DeclarationSeq : public ASTNode<DeclarationSeq, AST::Type::DeclarationSeq> {
    public:
    /// The declaration itself
    const Declaration* dec;
    /// The list
    DeclarationSeq *next, *last;

    /// Constructor
    DeclarationSeq(const Declaration* dec, DeclarationSeq* next) : dec(dec), next(next), last(next) {}

    /// Build
    static DeclarationSeq* build(ASTContainer& c, const AST* dec) { return new (c.allocate<DeclarationSeq>()) DeclarationSeq(Declaration::dynCast(dec), nullptr); }
    /// Append to list
    static DeclarationSeq* append(ASTContainer& c, const AST* ds, const AST* dec) {
        auto tds = const_cast<DeclarationSeq*>(dynCast(ds));
        auto newEntry = build(c, dec);
        if (tds->last)
            tds->last->next = newEntry;
        else
            tds->next = newEntry;
        tds->last = newEntry;
        return tds;
    }
};
//---------------------------------------------------------------------------
/// A translation unit
class TranslationUnit : public ASTNode<TranslationUnit, AST::Type::TranslationUnit> {
    public:
    /// The sequence
    const DeclarationSeq* ds;

    /// Constructor
    TranslationUnit(const DeclarationSeq* ds) : ds(ds) {}

    /// Build
    static TranslationUnit* build(ASTContainer& c, const AST* ds) { return new (c.allocate<TranslationUnit>()) TranslationUnit(DeclarationSeq::dynCast(ds)); }
};
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
#endif
