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
        ParameterDeclaration,
        ParameterDeclarationSeq,
        ParameterDeclarationList,
        UnnamedDeclaration,
        Contract,
        ContractSeq,
        ReturnList,
        FunctionType,
        UnqualifiedId,
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
    /// Cast helper
    static const T* dynCastOrNull(const AST* ast) { return ast ? dynCastHelper<T>(typeId, ast) : nullptr; }
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
/// Helper for lists
template <class T, class E, AST::Type typeId>
class ListHelper : public ASTNode<T, typeId> {
    public:
    /// The element
    const E* element;
    /// The list
    T *next, *last;

    /// Constructor
    ListHelper(const E* e) : element(e), next(nullptr), last(nullptr) {}

    /// Build
    static T* build(ASTContainer& c, const AST* e) { return new (c.allocate<T>()) T(E::dynCast(e)); }
    /// Append to list
    static T* append(ASTContainer& c, const AST* l, const AST* e) {
        auto tl = const_cast<T*>(T::dynCast(l));
        auto newEntry = build(c, e);
        if (tl->last)
            tl->last->next = newEntry;
        else
            tl->next = newEntry;
        tl->last = newEntry;
        return tl;
    }
};
//---------------------------------------------------------------------------
/// A list of declarations
class DeclarationSeq : public ListHelper<DeclarationSeq, Declaration, AST::Type::DeclarationSeq> {
};
//---------------------------------------------------------------------------
/// A parameter declaration
class ParameterDeclaration : public ASTNode<ParameterDeclaration, AST::Type::ParameterDeclaration> {
    public:
    /// The direction statement (if any)
    const Token* direction;
    /// The declaration itself
    const Declaration* decl;

    /// Constructor
    ParameterDeclaration(const Token* direction, const Declaration* decl) : direction(direction), decl(decl) {}

    /// Build
    static ParameterDeclaration* build(ASTContainer& c, const AST* direction, const AST* decl) { return new (c.allocate<ParameterDeclaration>()) ParameterDeclaration(Token::dynCastOrNull(direction), Declaration::dynCast(decl)); }
};
//---------------------------------------------------------------------------
/// A sequence of parameter declarations
class ParameterDeclarationSeq : public ListHelper<ParameterDeclarationSeq, ParameterDeclaration, AST::Type::ParameterDeclarationSeq> {
};
//---------------------------------------------------------------------------
/// Container for a parameter declaration list
class ParameterDeclarationList : public ASTNode<ParameterDeclarationList, AST::Type::ParameterDeclarationList> {
    public:
    /// The list itself
    const ParameterDeclarationSeq* list;

    /// Constructor
    ParameterDeclarationList(const ParameterDeclarationSeq* list) : list(list) {}

    /// Build
    static ParameterDeclarationList* build(ASTContainer& c, const AST* list) { return new (c.allocate<ParameterDeclarationList>()) ParameterDeclarationList(ParameterDeclarationSeq::dynCast(list)); }
};
//---------------------------------------------------------------------------
/// Return list
class ReturnList : public ASTNode<ReturnList, AST::Type::ReturnList> {
    public:
    /// Subtypes
    enum SubType : bool { Single,
                          Multiple };

    /// The subtype
    SubType subtype;
    /// The value
    const AST* value;

    /// Constructor
    ReturnList(SubType subtype, const AST* value) : subtype(subtype), value(value) {}

    /// Build
    static ReturnList* build(ASTContainer& c, SubType subtype, const AST* value) { return new (c.allocate<ReturnList>()) ReturnList(subtype, value); }
};
//---------------------------------------------------------------------------
/// A contract entry
class Contract : public ASTNode<Contract, AST::Type::Contract> {
    public:
    /// The kind
    const Token* kind;
    /// The value (if any)
    const AST* value;
    /// The condition
    const AST* condition;
    /// The comment (if any)
    const Token* comment;

    /// Constructor
    Contract(const Token* kind, const AST* value, const AST* condition, const Token* comment) : kind(kind), value(value), condition(condition), comment(comment) {}

    /// Build
    static Contract* build(ASTContainer& c, const AST* kind, const AST* value, const AST* condition, const AST* comment) { return new (c.allocate<Contract>()) Contract(Token::dynCast(kind), value, condition, Token::dynCastOrNull(comment)); }
};
//---------------------------------------------------------------------------
/// A sequence of contracts
class ContractSeq : public ListHelper<ContractSeq, Contract, AST::Type::ContractSeq> {
};
//---------------------------------------------------------------------------
/// A function type
class FunctionType : public ASTNode<FunctionType, AST::Type::FunctionType> {
    public:
    /// The parameters
    const ParameterDeclarationList* parameter;
    /// Throws specifier (if any)
    const Token* throws;
    /// The return list (if any)
    const ReturnList* returnList;
    /// The contract list (if any)
    const ContractSeq* contractSeq;

    /// Constructor
    FunctionType(const ParameterDeclarationList* parameter, const Token* throws, const ReturnList* returnList, const ContractSeq* contractSeq) : parameter(parameter), throws(throws), returnList(returnList), contractSeq(contractSeq) {}

    /// Build
    static FunctionType* build(ASTContainer& c, const AST* parameter, const AST* throws, const AST* returnList, const AST* contractSeq) { return new (c.allocate<FunctionType>()) FunctionType(ParameterDeclarationList::dynCast(parameter), Token::dynCastOrNull(throws), ReturnList::dynCastOrNull(returnList), ContractSeq::dynCastOrNull(contractSeq)); }
};
//---------------------------------------------------------------------------
/// An unqualified if
class UnqualifiedId : public ASTNode<UnqualifiedId, AST::Type::UnqualifiedId> {
    public:
    /// Possible types
    enum SubType : bool { Normal,
                          Template };

    /// The type
    SubType type;
    /// The const indicator (if any)
    const Token* constFlag;
    /// The id itself
    const AST* id;

    /// Constructor
    UnqualifiedId(SubType type, const Token* constFlag, const AST* id) : type(type), constFlag(constFlag), id(id) {}

    /// Build
    static UnqualifiedId* build(ASTContainer& c, SubType subType, const AST* constFlag, const AST* id) { return new (c.allocate<UnqualifiedId>()) UnqualifiedId(subType, Token::dynCastOrNull(constFlag), id); }
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
