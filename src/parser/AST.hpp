#ifndef H_AST
#define H_AST
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
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
        CharLiteral,
        CompoundStatement,
        ConststExpr,
        Contract,
        ContractKind,
        ContractSeq,
        DecimalFloat,
        DecimalInteger,
        Declaration,
        DeclarationSeq,
        DeclarationStatement,
        DoWhileStatement,
        DotExpression,
        ExpressionList,
        ExpressionStatement,
        ForStatement,
        FunctionType,
        FundamentalType,
        FundamentalTypeModifier,
        FundamentalTypeModifierList,
        HexFloat,
        HexInteger,
        Identifier,
        InspectExpression,
        OctalInteger,
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
        SelectStatement,
        StatementSeq,
        StringLiteral,
        TemplateArgument,
        TemplateArgumentList,
        TemplateId,
        Token,
        TranslationUnit,
        TypeModifier,
        UnnamedDeclaration,
        UnqualifiedId,
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
enum Alternative { Is,
                   As };
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
enum BinaryExpression { LogicalAnd,
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
                        Modulo };
enum ContractKind { Pre,
                    Post,
                    Assert };
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
enum PrefixExpression { Not,
                        UPlus,
                        UMinus };
enum ReturnList { Single,
                  Multiple };
enum UnnamedDeclaration { Function,
                          Value };
enum UnqualifiedId { Normal,
                     ConstNormal,
                     Template,
                     ConstTemplate };
enum TemplateArgument { Expression,
                        IdExpression };
enum TypeModifier { Pointer };
enum QualifiedId { Nested,
                   Member };
//---------------------------------------------------------------------------
}
}
//---------------------------------------------------------------------------
#endif
