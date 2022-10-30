#ifndef H_AST
#define H_AST
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
#include "parser/Range.hpp"
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
class Source;
//---------------------------------------------------------------------------
/// An AST node
class AST {
    public:
    /// Known types
    enum Type : uint8_t {
        Alternative,
        AlternativeSeq,
        AssignmentExpression,
        AssignmentOperator,
        BinaryExpression,
        BracketExpression,
        Class,
        CompoundStatement,
        ConststExpr,
        Contract,
        ContractKind,
        ContractSeq,
        Declaration,
        DeclarationSeq,
        DeclarationStatement,
        DoWhileStatement,
        DotExpression,
        ExpressionList,
        ExpressionListExpression,
        ExpressionStatement,
        ForStatement,
        FunctionModifier,
        FunctionModifierList,
        FunctionType,
        FundamentalType,
        FundamentalTypeModifier,
        FundamentalTypeModifierList,
        Identifier,
        InspectExpression,
        Literal,
        Namespace,
        NestedNameSpecifier,
        NestedNameSpecifierList,
        NewExpression,
        OperatorName,
        ParameterDeclaration,
        ParameterDeclarationList,
        ParameterDeclarationSeq,
        ParameterDirection,
        ParenExpression,
        PostfixExpression,
        PrefixExpression,
        QualifiedId,
        ReturnList,
        ReturnStatement,
        SelectionStatement,
        StatementSeq,
        Template,
        TemplateArgument,
        TemplateArgumentList,
        TemplateId,
        TemplateDeclarationList,
        Token,
        TranslationUnit,
        TypeModifier,
        UnnamedDeclaration,
        UnqualifiedId,
        Using,
        UsingDecltype,
        WhileStatement
    };

    private:
    /// The type
    Type type;
    /// Flags
    uint8_t flags;
    /// The number of children
    uint16_t childCount;
    /// The subtype
    unsigned subType;
    /// The source range
    Range range;
    /// The children
    const AST* children[];

    friend class Source;

    public:
    /// Constructor
    explicit AST(Type type, uint8_t flags, uint16_t childCount, unsigned subType, Range range) : type(type), flags(flags), childCount(childCount), subType(subType), range(range) {}

    /// Get the node type
    Type getType() const { return type; }
    /// Get the sub type as specific enum
    template <class T>
    T getSubType() const { return static_cast<T>(subType); }
    /// Get the range within the source text
    Range getRange() const { return range; }

    /// Access an element and make sure that the node type matches. Accessing a null pointer is an error
    const AST* get(unsigned element, Type nodeType) const;
    /// Access an element and make sure that the node type matches. A null pointer is valid
    const AST* getOrNull(unsigned element, Type nodeType) const;
    /// Access an element with enforcing a specific node type
    const AST* getAny(unsigned element) const;
    /// Access an element with enforcing a specific node type
    const AST* getAnyOrNull(unsigned element) const;
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
};
//---------------------------------------------------------------------------
namespace ast {
//---------------------------------------------------------------------------
enum Alternative { MatchIs,
                   MatchAs };
enum AssignmentOperator { Assignment,
                          MultiplyEq,
                          SlashEq,
                          ModuloEq,
                          PlusEq,
                          MinusEq,
                          RightShiftEq,
                          LeftShiftEq,
                          AmpersandEq,
                          CaretEq,
                          PipeEq };
enum BinaryExpression {
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
    Modulo,
    Is,
    As
};
enum ContractKind { Pre,
                    Post,
                    Assert };
enum class FunctionModifier {
    In,
    Out,
    Inout,
    Move,
    Forward,
    Static,
    Virtual,
    Abstract,
    Override,
    Final,
    Defaulted,
    Deleted
};
enum FundamentalType { Void,
                       Char,
                       Char8,
                       Char16,
                       Char32,
                       WChar,
                       Int,
                       Bool,
                       Float,
                       Double,
                       LongDouble };
enum FundamentalTypeModifier { Signed,
                               Unsigned,
                               Long,
                               Short };
enum Literal {
    Nullptr,
    True,
    False,
    CharLiteral,
    StringLiteral,
    DecimalFloat,
    DecimalInteger,
    HexFloat,
    HexInteger,
    OctalInteger
};
enum NestedNameSpecifier { Absolute,
                           Relative };
enum ParameterDirection { In,
                          Out,
                          Inout,
                          Copy,
                          Move,
                          Forward };
enum PostfixExpression { PlusPlus,
                         MinusMinus,
                         Ref,
                         Deref,
                         Compl,
                         Dollar };
enum PrefixExpression {
    Not,
    UPlus,
    UMinus,
    Typeid
};
enum ReturnList { Single,
                  Multiple };
enum UnnamedDeclaration { Function,
                          Value };
enum UnqualifiedId {
    Normal,
    Template,
};
enum TemplateArgument { Expression,
                        IdExpression };
enum TypeModifier {
    Const,
    Pointer
};
enum QualifiedId { Nested,
                   Member };
enum class OperatorName {
    BitAnd,
    BitAndEq,
    BitOr,
    BitOrEq,
    BitXor,
    BitXorEq,
    Complement,
    Div,
    DivEq,
    Equal,
    Greater,
    GreaterEq,
    LeftShift,
    LeftShiftEq,
    Less,
    LessEq,
    LogicalAnd,
    LogicalOr,
    Minus,
    MinusEq,
    MinusMinus,
    Modulo,
    ModuloEq,
    Mul,
    MulEq,
    Not,
    NotEqual,
    Plus,
    PlusEq,
    PlusPlus,
    RightShift,
    RightShiftEq,
    Spaceship
};
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
#endif
