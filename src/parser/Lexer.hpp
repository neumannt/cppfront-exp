#ifndef H_Lexer
#define H_Lexer
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
/// A lexer
class Lexer {
    public:
    /// The source location
    struct SourceLocation {
        unsigned line = 0, column = 0, byteOfs = 0;
    };
    /// An error description
    struct Error {
        /// The occurrence in the source
        SourceLocation loc;
        /// The text
        std::string text;
    };
    /// Known tokens
    enum class Token : unsigned {
        Eof,
        Error,
        Ampersand,
        AmpersandEq,
        Arrow,
        Assignment,
        BinaryLiteral,
        Carret,
        CarretEq,
        CharacterLiteral,
        Colon,
        Comma,
        DecimalLiteral,
        Dollar,
        Dot,
        Ellipsis,
        EqualComparison,
        FloatLiteral,
        Greater,
        GreaterEq,
        HexadecimalLiteral,
        Identifier,
        Keyword,
        LeftBrace,
        LeftBracket,
        LeftParen,
        LeftShift,
        LeftShiftEq,
        Less,
        LessEq,
        LogicalAnd,
        LogicalAndEq,
        LogicalOr,
        LogicalOrEq,
        Minus,
        MinusEq,
        MinusMinus,
        Modulo,
        ModuloEq,
        Multiply,
        MultiplyEq,
        Not,
        NotEqualComparison,
        OctalLiteral,
        Pipe,
        PipeEq,
        Plus,
        PlusEq,
        PlusPlus,
        QuestionMark,
        RightBrace,
        RightBracket,
        RightParen,
        Scope,
        Semicolon,
        Slash,
        SlashEq,
        Spaceship,
        StringLiteral,
        Tilde,
        TildeEq,
    };
    /// Known keywords
    enum class Keyword : unsigned {
        Alignas,
        Alignof,
        Asm,
        As,
        Auto,
        Bool,
        Break,
        Case,
        Catch,
        Char,
        Char16_t,
        Char32_t,
        Char8_t,
        Class,
        Co_await,
        Co_return,
        Co_yield,
        Concept,
        Const,
        Const_cast,
        Consteval,
        Constinit,
        Continue,
        Delete,
        Decltype,
        Default,
        Double,
        Do,
        Dynamic_cast,
        Else,
        Enum,
        Explicit,
        Export,
        Extern,
        False,
        Float,
        For,
        Friend,
        Goto,
        If,
        Import,
        Inline,
        Int,
        Is,
        Long,
        Module,
        Mutable,
        Namespace,
        New,
        Noexcept,
        Nullptr,
        Operator,
        Private,
        Protected,
        Public,
        Register,
        Reinterpret_cast,
        Requires,
        Return,
        Short,
        Signed,
        Sizeof,
        Static,
        Static_assert,
        Static_cast,
        Struct,
        Switch,
        Template,
        This,
        Thread_local,
        Throws,
        Throw,
        True,
        Try,
        Typedef,
        Typeid,
        Typename,
        Union,
        Unsigned,
        Using,
        Virtual,
        Void,
        Volatile,
        Wchar_t,
        While
    };

    private:
    /// The input
    std::string input;
    /// The errors
    std::vector<Error> errors;
    /// The comments
    std::vector<std::pair<SourceLocation, std::string_view>> comments;
    /// The current position
    SourceLocation loc;
    /// The last token start
    SourceLocation tokenStart;

    /// Examine the next character
    inline char peek(unsigned ofs = 0) {
        return (loc.byteOfs + ofs < input.size()) ? input[loc.byteOfs + ofs] : 0;
    }
    /// Consume character(s) and update the position
    inline void consume(unsigned c = 1) {
        loc.byteOfs += c;
        loc.column += c;
    }

    /// Store an error message
    void addError(SourceLocation loc, std::string text);
    /// Store a comment
    void addComment(SourceLocation loc, std::string_view text);
    /// Process a character in a comment or a string
    void processBlockChar();
    /// Handle a comment starting with //
    void handleSingleLineComment();
    /// Handle a comment starting with /* ... */
    void handleMultiLineComment();
    /// Handle potential integer suffix
    void handleIntegerSuffix();
    /// Handle potential float suffix
    void handleFloatSuffix();
    /// Handle a literal starting with 0b
    Token handleBinaryLiteral();
    /// Handle a literal starting with 0x
    Token handleHexLiteral();
    /// Handle a literal starting with 0[0-7]
    Token handleOctalLiteral();
    /// Handle a decimal literal
    Token handleDecimalLiteral();
    /// Check for string prefix
    unsigned isStringPrefix();
    /// Handle a string literal
    Token handleStringLiteral(unsigned prefixLen);
    /// Handle an identifier
    Token handleIdentifier();

    public:
    /// Constructor
    Lexer();
    /// Destructor
    ~Lexer();

    /// Load the input from a file, reset the lexer
    bool loadFile(const std::string& fileName);
    /// Get the error list
    auto& getErrors() const { return errors; }

    /// Get the next token
    Token next();
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
