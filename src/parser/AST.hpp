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
        TemplateArgument,
        TemplateArgumentList,
        TemplateId,
        QualifiedId,
        ExpressionList,
        AssignmentExpression,
        BinaryExpression,
        PrefixExpression,
        PostfixExpression,
        BracketExpression,
        ParenExpression,
        DotExpression,
        Alternative,
        AlternativeSeq,
        InspectExpression,
        Literal,
        ExpressionStatement,
        StatementSeq,
        CompoundStatement,
        SelectStatement,
        DeclarationStatement,
        ReturnStatement,
        WhileStatement,
        DoWhileStatement,
        ForStatement,
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
        auto newEntry = T::build(c, e);
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
/// A template argument
class TemplateArgument : public ASTNode<TemplateArgument, AST::Type::TemplateArgument> {
    public:
    /// Subtypes
    enum SubType { Expression };

    /// The subtype
    SubType subType;
    /// The expression
    const AST* expression;

    /// Constructor
    TemplateArgument(SubType subType, const AST* expression) : subType(subType), expression(expression) {}

    /// Build
    static TemplateArgument* build(ASTContainer& c, SubType subType, const AST* expression) { return new (c.allocate<TemplateArgument>()) TemplateArgument(subType, expression); }
};
//---------------------------------------------------------------------------
/// A list of template arguments
class TemplateArgumentList : public ListHelper<TemplateArgumentList, TemplateArgument, AST::Type::TemplateArgumentList> {
};
//---------------------------------------------------------------------------
/// An template id
class TemplateId : public ASTNode<TemplateId, AST::Type::TemplateId> {
    public:
    /// The identifier
    const Token* identifier;
    /// The arguments (if any)
    const TemplateArgumentList* arguments;

    /// Constructor
    TemplateId(const Token* identifier, const TemplateArgumentList* arguments) : identifier(identifier), arguments(arguments) {}

    /// Build
    static TemplateId* build(ASTContainer& c, const AST* identifier, const AST* arguments) { return new (c.allocate<TemplateId>()) TemplateId(Token::dynCast(identifier), TemplateArgumentList::dynCastOrNull(arguments)); }
};
//---------------------------------------------------------------------------
/// A qualified id
class QualifiedId : public ASTNode<QualifiedId, AST::Type::QualifiedId> {
    public:
    /// Subtypes
    enum SubType : bool { Nested,
                          Member };
    /// The subtype
    SubType subType;
    /// The scope
    const AST* scope;
    /// The id
    const UnqualifiedId* id;

    /// Constructor
    QualifiedId(SubType subType, const AST* scope, const UnqualifiedId* id) : subType(subType), scope(scope), id(id) {}

    /// Build
    static QualifiedId* build(ASTContainer& c, SubType subType, const AST* scope, const AST* id) { return new (c.allocate<QualifiedId>()) QualifiedId(subType, scope, UnqualifiedId::dynCast(id)); }
};
//---------------------------------------------------------------------------
/// A list of expressions
class ExpressionList : public ListHelper<ExpressionList, AST, AST::Type::ExpressionList> {
    public:
    /// Build
    static ExpressionList* build(ASTContainer& c, const AST* e) { return new (c.allocate<ExpressionList>()) ExpressionList(e); }
};
//---------------------------------------------------------------------------
/// An assignment expression
class AssignmentExpression : public ASTNode<AssignmentExpression, AST::Type::AssignmentExpression> {
    public:
    /// The target
    const AST* target;
    /// The operator
    const Token* op;
    /// The value
    const AST* value;

    /// Constructor
    AssignmentExpression(const AST* target, const Token* op, const AST* value) : target(target), op(op), value(value) {}

    /// Build
    static AssignmentExpression* build(ASTContainer& c, const AST* target, const AST* op, const AST* value) { return new (c.allocate<AssignmentExpression>()) AssignmentExpression(target, Token::dynCast(op), value); }
};
//---------------------------------------------------------------------------
/// A binary expression
class BinaryExpression : public ASTNode<BinaryExpression, AST::Type::BinaryExpression> {
    public:
    /// The subtype
    enum SubType {
        LogicalAnd,
        LogicalOr,
        BitAnd,
        BitOr,
        BitXor,
        Equal,
        NotEqual,
        Less,
        LessEq,
        Greater,
        GreaterEq,
        Spaceship,
        LeftShift,
        RightShift,
        Plus,
        Minus,
        Mul,
        Div,
        Modulo
    };
    /// The subtype
    SubType subType;
    /// The arguments
    const AST *left, *right;

    /// Constructor
    BinaryExpression(SubType subType, const AST* left, const AST* right) : subType(subType), left(left), right(right) {}

    /// Build
    static BinaryExpression* build(ASTContainer& c, SubType subType, const AST* left, const AST* right) { return new (c.allocate<BinaryExpression>()) BinaryExpression(subType, left, right); }
};
//---------------------------------------------------------------------------
/// A prefix expression
class PrefixExpression : public ASTNode<PrefixExpression, AST::Type::PrefixExpression> {
    public:
    /// The operator
    const Token* op;
    /// The expression
    const AST* expression;

    /// Constructor
    PrefixExpression(const Token* op, const AST* expression) : op(op), expression(expression) {}

    /// Build
    static PrefixExpression* build(ASTContainer& c, const AST* op, const AST* expression) { return new (c.allocate<PrefixExpression>()) PrefixExpression(Token::dynCast(op), expression); }
};
//---------------------------------------------------------------------------
/// A postfix expression
class PostfixExpression : public ASTNode<PrefixExpression, AST::Type::PostfixExpression> {
    public:
    /// The expression
    const AST* expression;
    /// The operator
    const Token* op;

    /// Constructor
    PostfixExpression(const AST* expression, const Token* op) : expression(expression), op(op) {}

    /// Build
    static PostfixExpression* build(ASTContainer& c, const AST* expression, const AST* op) { return new (c.allocate<PostfixExpression>()) PostfixExpression(expression, Token::dynCast(op)); }
};
//---------------------------------------------------------------------------
/// A bracket expression
class BracketExpression : public ASTNode<BracketExpression, AST::Type::BracketExpression> {
    public:
    /// The base
    const AST* base;
    /// The arguments
    const ExpressionList* arguments;

    /// Constructor
    BracketExpression(const AST* base, const ExpressionList* arguments) : base(base), arguments(arguments) {}

    /// Build
    static BracketExpression* build(ASTContainer& c, const AST* base, const AST* arguments) { return new (c.allocate<BracketExpression>()) BracketExpression(base, ExpressionList::dynCast(arguments)); }
};
//---------------------------------------------------------------------------
/// A parenthesis expression
class ParenExpression : public ASTNode<ParenExpression, AST::Type::ParenExpression> {
    public:
    /// The base
    const AST* base;
    /// The arguments
    const ExpressionList* arguments;

    /// Constructor
    ParenExpression(const AST* base, const ExpressionList* arguments) : base(base), arguments(arguments) {}

    /// Build
    static ParenExpression* build(ASTContainer& c, const AST* base, const AST* arguments) { return new (c.allocate<ParenExpression>()) ParenExpression(base, ExpressionList::dynCast(arguments)); }
};
//---------------------------------------------------------------------------
/// A dot expression
class DotExpression : public ASTNode<DotExpression, AST::Type::DotExpression> {
    public:
    /// The base
    const AST* base;
    /// The id
    const AST* id;

    /// Constructor
    DotExpression(const AST* base, const AST* id) : base(base), id(id) {}

    /// Build
    static DotExpression* build(ASTContainer& c, const AST* base, const AST* id) { return new (c.allocate<DotExpression>()) DotExpression(base, id); }
};
//---------------------------------------------------------------------------
/// An alternative
class Alternative : public ASTNode<Alternative, AST::Type::Alternative> {
    public:
    /// Sub type
    enum SubType : bool { Is,
                          As };
    /// The subtype
    SubType subType;
    /// The name (if any)
    const UnqualifiedId* name;
    /// The constraint
    const AST* constraint;
    /// The statement
    const AST* statement;

    /// Constructor
    Alternative(SubType subType, const UnqualifiedId* name, const AST* constraint, const AST* statement) : subType(subType), name(name), constraint(constraint), statement(statement) {}

    /// Build
    static Alternative* build(ASTContainer& c, SubType subType, const AST* name, const AST* constraint, const AST* statement) { return new (c.allocate<Alternative>()) Alternative(subType, UnqualifiedId::dynCastOrNull(name), constraint, statement); }
};
//---------------------------------------------------------------------------
/// A sequence of alternatives
class AlternativeSeq : public ListHelper<AlternativeSeq, Alternative, AST::Type::AlternativeSeq> {
};
//---------------------------------------------------------------------------
/// An inspect expression
class InspectExpression : public ASTNode<InspectExpression, AST::Type::InspectExpression> {
    public:
    /// The constexp marker (if any)
    const Token* constexprMarker;
    /// The expression
    const AST* expression;
    /// The id (if any)
    const AST* id;
    /// The alternatives
    const AlternativeSeq* alternatives;

    /// Constructor
    InspectExpression(const Token* constexprMarker, const AST* expression, const AST* id, const AlternativeSeq* alternatives) : constexprMarker(constexprMarker), expression(expression), id(id), alternatives(alternatives) {}

    /// Build
    static InspectExpression* build(ASTContainer& c, const AST* constexprMarker, const AST* expression, const AST* id, const AST* alternatives) { return new (c.allocate<InspectExpression>()) InspectExpression(Token::dynCastOrNull(constexprMarker), expression, id, AlternativeSeq::dynCast(alternatives)); }
};
//---------------------------------------------------------------------------
/// A literal
class Literal : public ASTNode<Literal, AST::Type::Literal> {
    public:
    /// The subtype
    enum SubType {
        Identifier,
        Decimal,
        Float,
        String,
        Char,
        Binary,
        Octal,
        Hex
    };
    /// The subtype
    SubType subType;
    /// The value
    const Token* value;

    /// Constructor
    Literal(SubType subType, const Token* value) : subType(subType), value(value) {}

    /// Build
    static Literal* build(ASTContainer& c, SubType subType, const AST* value) { return new (c.allocate<Literal>()) Literal(subType, Token::dynCast(value)); }
};
//---------------------------------------------------------------------------
/// A sequence of statements
class StatementSeq : public ListHelper<StatementSeq, AST, AST::Type::StatementSeq> {
    public:
    /// Build
    static StatementSeq* build(ASTContainer& c, const AST* s) { return new (c.allocate<StatementSeq>()) StatementSeq(s); }
};
//---------------------------------------------------------------------------
/// An expression statement
class ExpressionStatement : public ASTNode<ExpressionStatement, AST::Type::ExpressionStatement> {
    public:
    /// The expression
    const AST* expression;

    /// Constructor
    ExpressionStatement(const AST* expression) : expression(expression) {}

    /// Build
    static ExpressionStatement* build(ASTContainer& c, const AST* expression) { return new (c.allocate<ExpressionStatement>()) ExpressionStatement(expression); }
};
//---------------------------------------------------------------------------
/// A compound statement
class CompoundStatement : public ASTNode<CompoundStatement, AST::Type::CompoundStatement> {
    public:
    /// The statements (if any)
    const StatementSeq* statements;

    /// Constructor
    CompoundStatement(const StatementSeq* statements) : statements(statements) {}

    /// Build
    static CompoundStatement* build(ASTContainer& c, const AST* statements) { return new (c.allocate<CompoundStatement>()) CompoundStatement(StatementSeq::dynCastOrNull(statements)); }
};
//---------------------------------------------------------------------------
/// A select statement
class SelectStatement : public ASTNode<SelectStatement, AST::Type::SelectStatement> {
    public:
    /// The constexpr marker (if any)
    const Token* constexprMarker;
    /// The condition
    const AST* condition;
    /// The then part
    const AST* thenPart;
    /// The else part (if any)
    const AST* elsePart;

    /// Constructor
    SelectStatement(const Token* constexprMarker, const AST* condition, const AST* thenPart, const AST* elsePart) : constexprMarker(constexprMarker), condition(condition), thenPart(thenPart), elsePart(elsePart) {}

    /// Build
    static SelectStatement* build(ASTContainer& c, const AST* constexprMarker, const AST* condition, const AST* thenPart, const AST* elsePart = nullptr) { return new (c.allocate<SelectStatement>()) SelectStatement(Token::dynCastOrNull(constexprMarker), condition, thenPart, elsePart); }
};
//---------------------------------------------------------------------------
/// A declaration statement
class DeclarationStatement : public ASTNode<DeclarationStatement, AST::Type::DeclarationStatement> {
    public:
    /// The declaration
    const Declaration* declaration;

    /// Constructor
    DeclarationStatement(const Declaration* declaration) : declaration(declaration) {}

    /// Build
    static DeclarationStatement* build(ASTContainer& c, const AST* declaration) { return new (c.allocate<DeclarationStatement>()) DeclarationStatement(Declaration::dynCast(declaration)); }
};
//---------------------------------------------------------------------------
/// A return statement
class ReturnStatement : public ASTNode<ReturnStatement, AST::Type::ReturnStatement> {
    public:
    /// The expression (if any)
    const AST* expression;

    /// Constructor
    ReturnStatement(const AST* expression) : expression(expression) {}

    /// Build
    static ReturnStatement* build(ASTContainer& c, const AST* expression) { return new (c.allocate<ReturnStatement>()) ReturnStatement(expression); }
};
//---------------------------------------------------------------------------
/// A while statement
class WhileStatement : public ASTNode<WhileStatement, AST::Type::WhileStatement> {
    public:
    /// The condition
    const AST* condition;
    /// The next statement (if any)
    const AST* next;
    /// The body
    const AST* body;

    /// Constructor
    WhileStatement(const AST* condition, const AST* next, const AST* body) : condition(condition), next(next), body(body) {}

    /// Build
    static WhileStatement* build(ASTContainer& c, const AST* condition, const AST* next, const AST* body) { return new (c.allocate<WhileStatement>()) WhileStatement(condition, next, body); }
};
//---------------------------------------------------------------------------
/// A do-while statement
class DoWhileStatement : public ASTNode<WhileStatement, AST::Type::WhileStatement> {
    public:
    /// The body
    const AST* body;
    /// The condition
    const AST* condition;
    /// The next statement (if any)
    const AST* next;

    /// Constructor
    DoWhileStatement(const AST* body, const AST* condition, const AST* next) : body(body), condition(condition), next(next) {}

    /// Build
    static DoWhileStatement* build(ASTContainer& c, const AST* body, const AST* condition, const AST* next) { return new (c.allocate<DoWhileStatement>()) DoWhileStatement(body, condition, next); }
};
//---------------------------------------------------------------------------
/// A for statement
class ForStatement : public ASTNode<ForStatement, AST::Type::ForStatement> {
    public:
    /// The value
    const AST* value;
    /// The next statement (if any)
    const AST* next;
    /// The body
    const AST* body;

    /// Constructor
    ForStatement(const AST* value, const AST* next, const AST* body) : value(value), next(next), body(body) {}

    /// Build
    static ForStatement* build(ASTContainer& c, const AST* value, const AST* next, const AST* body) { return new (c.allocate<ForStatement>()) ForStatement(value, next, body); }
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
