#include "Lexer.hpp"
#include "AST.hpp"
#include "Parser.hpp"
#include <unordered_map>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: BSD-3-Clause
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace {
//---------------------------------------------------------------------------
/// C++ keywords, including the cppfront keywords is and as
static const unordered_map<string_view, Lexer::Keyword> keywords{
    {"alignas"sv, Lexer::Keyword::Alignas}, {"alignof"sv, Lexer::Keyword::Alignof}, {"asm"sv, Lexer::Keyword::Asm}, {"as"sv, Lexer::Keyword::As}, {"auto"sv, Lexer::Keyword::Auto}, {"bool"sv, Lexer::Keyword::Bool}, {"break"sv, Lexer::Keyword::Break}, {"case"sv, Lexer::Keyword::Case}, {"catch"sv, Lexer::Keyword::Catch}, {"char"sv, Lexer::Keyword::Char}, {"char16_t"sv, Lexer::Keyword::Char16_t}, {"Char32_t"sv, Lexer::Keyword::Char32_t}, {"char8_t"sv, Lexer::Keyword::Char8_t}, {"class"sv, Lexer::Keyword::Class}, {"co_await"sv, Lexer::Keyword::Co_await}, {"co_return"sv, Lexer::Keyword::Co_return}, {"co_yield"sv, Lexer::Keyword::Co_yield}, {"concept"sv, Lexer::Keyword::Concept}, {"const"sv, Lexer::Keyword::Const}, {"const_cast"sv, Lexer::Keyword::Const_cast}, {"consteval"sv, Lexer::Keyword::Consteval}, {"constinit"sv, Lexer::Keyword::Constinit}, {"continue"sv, Lexer::Keyword::Continue}, {"decltype"sv, Lexer::Keyword::Decltype}, {"default"sv, Lexer::Keyword::Default}, {"delete"sv, Lexer::Keyword::Delete}, {"double"sv, Lexer::Keyword::Double}, {"do"sv, Lexer::Keyword::Do}, {"dynamic_cast"sv, Lexer::Keyword::Dynamic_cast}, {"else"sv, Lexer::Keyword::Else}, {"enum"sv, Lexer::Keyword::Enum}, {"explicit"sv, Lexer::Keyword::Explicit}, {"export"sv, Lexer::Keyword::Export}, {"extern"sv, Lexer::Keyword::Extern}, {"false"sv, Lexer::Keyword::False}, {"float"sv, Lexer::Keyword::Float}, {"for"sv, Lexer::Keyword::For}, {"friend"sv, Lexer::Keyword::Friend}, {"goto"sv, Lexer::Keyword::Goto}, {"if"sv, Lexer::Keyword::If}, {"import"sv, Lexer::Keyword::Import}, {"inline"sv, Lexer::Keyword::Inline}, {"int"sv, Lexer::Keyword::Int}, {"is"sv, Lexer::Keyword::Is}, {"long"sv, Lexer::Keyword::Long}, {"module"sv, Lexer::Keyword::Module}, {"mutable"sv, Lexer::Keyword::Mutable}, {"namespace"sv, Lexer::Keyword::Namespace}, {"new"sv, Lexer::Keyword::New}, {"noexcept"sv, Lexer::Keyword::Noexcept}, {"nullptr"sv, Lexer::Keyword::Nullptr}, {"operator"sv, Lexer::Keyword::Operator}, {"private"sv, Lexer::Keyword::Private}, {"protected"sv, Lexer::Keyword::Protected}, {"public"sv, Lexer::Keyword::Public}, {"register"sv, Lexer::Keyword::Register}, {"reinterpret_cast"sv, Lexer::Keyword::Reinterpret_cast}, {"requires"sv, Lexer::Keyword::Requires}, {"return"sv, Lexer::Keyword::Return}, {"short"sv, Lexer::Keyword::Short}, {"signed"sv, Lexer::Keyword::Signed}, {"sizeof"sv, Lexer::Keyword::Sizeof}, {"static"sv, Lexer::Keyword::Static}, {"static_assert"sv, Lexer::Keyword::Static_assert}, {"static_cast"sv, Lexer::Keyword::Static_cast}, {"struct"sv, Lexer::Keyword::Struct}, {"switch"sv, Lexer::Keyword::Switch}, {"template"sv, Lexer::Keyword::Template}, {"this"sv, Lexer::Keyword::This}, {"thread_local"sv, Lexer::Keyword::Thread_local}, {"throws"sv, Lexer::Keyword::Throws}, {"throw"sv, Lexer::Keyword::Throw}, {"true"sv, Lexer::Keyword::True}, {"try"sv, Lexer::Keyword::Try}, {"typedef"sv, Lexer::Keyword::Typedef}, {"typeid"sv, Lexer::Keyword::Typeid}, {"typename"sv, Lexer::Keyword::Typename}, {"union"sv, Lexer::Keyword::Union}, {"unsigned"sv, Lexer::Keyword::Unsigned}, {"using"sv, Lexer::Keyword::Using}, {"virtual"sv, Lexer::Keyword::Virtual}, {"void"sv, Lexer::Keyword::Void}, {"volatile"sv, Lexer::Keyword::Volatile}, {"wchar_t"sv, Lexer::Keyword::Wchar_t}, {"while"sv, Lexer::Keyword::While}};
//---------------------------------------------------------------------------
/// Alternative token for operators
enum class AltToken : unsigned {
    And,
    Bitor,
    Or,
    Xor,
    Compl,
    Bitand,
    And_eq,
    Or_eq,
    Xor_eq,
    Not,
    Not_eq
};
//---------------------------------------------------------------------------
/// Alternative tokens list
static const unordered_map<string_view, AltToken> alternativeTokens{
    {"and"sv, AltToken::And}, {"bitor"sv, AltToken::Bitor}, {"or"sv, AltToken::Or}, {"xor"sv, AltToken::Xor}, {"compl"sv, AltToken::Compl}, {"bitand"sv, AltToken::Bitand}, {"and_eq"sv, AltToken::And_eq}, {"or_eq"sv, AltToken::Or_eq}, {"xor_eq"sv, AltToken::Xor_eq}, {"not"sv, AltToken::Not}, {"not_eq"sv, AltToken::Not_eq}};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
Lexer::Lexer(vector<Error>& errors)
    : errors(errors)
// Constructor
{
}
//---------------------------------------------------------------------------
Lexer::~Lexer()
// Destructor
{
}
//---------------------------------------------------------------------------
void Lexer::addError(SourceLocation loc, string text)
// Store an error message
{
    errors.emplace_back(loc, move(text));
}
//---------------------------------------------------------------------------
void Lexer::addComment(SourceLocation loc, string_view text)
// Store a comment
{
    comments.emplace_back(loc, text);
}
//---------------------------------------------------------------------------
void Lexer::reset(string_view newInput)
// Reset the lexer
{
    input = newInput;
    loc = {1, 1, 0};
}
//---------------------------------------------------------------------------
static constexpr unsigned tabWidth = 8;
//---------------------------------------------------------------------------
void Lexer::handleSingleLineComment()
// Handle a comment starting with //
{
    unsigned limit = input.length();
    while ((loc.byteOfs < limit) && (input[loc.byteOfs] != '\n') && (input[loc.byteOfs] != '\r')) consume();
    addComment(tokenStart, string_view(input.data() + tokenStart.byteOfs, input.data() + loc.byteOfs));
}
//---------------------------------------------------------------------------
void Lexer::processBlockChar()
// Process a character in a comment or a string
{
    char c1 = input[loc.byteOfs];
    consume();
    if (c1 == '\n') {
        if (peek(0) == '\r') loc.byteOfs++;
        loc.line++;
        loc.column = 1;
    } else if (c1 == '\r') {
        if (peek(0) == '\n') loc.byteOfs++;
        loc.line++;
        loc.column = 1;
    } else if (c1 == '\t') {
        if ((loc.column - 1) % tabWidth) loc.column += tabWidth - ((loc.column - 1) % tabWidth);
    }
}
//---------------------------------------------------------------------------
void Lexer::handleMultiLineComment()
// Handle a comment starting with //
{
    unsigned limit = input.length();
    consume(); // consume the second slash
    while (true) {
        unsigned ofs = loc.byteOfs;
        if (ofs + 1 >= limit) {
            addError(tokenStart, "unterminated multi-line comment");
            loc.byteOfs = limit;
            return;
        }
        char c1 = input[ofs], c2 = input[ofs + 1];
        if ((c1 == '*') && (c2 == '/')) {
            consume(2);
            break;
        }
        processBlockChar();
    }
    addComment(tokenStart, string_view(input.data() + tokenStart.byteOfs, input.data() + loc.byteOfs));
}
//---------------------------------------------------------------------------
void Lexer::handleIntegerSuffix()
// Handle potential integer suffix
{
    auto tryU = [this]() {
        char c = peek();
        if ((c == 'u') || (c == 'U')) {
            consume();
            return true;
        }
        return false;
    };
    auto tryL = [this]() {
        char c = peek();
        if ((c == 'l') || (c == 'L')) {
            consume();
            c = peek();
            if ((c == 'l') || (c == 'L')) consume();
            return true;
        } else if ((c == 'z') || (c == 'Z')) {
            consume();
            return true;
        }
        return false;
    };
    if (tryU()) {
        tryL();
    } else if (tryL()) {
        tryU();
    }
}
//---------------------------------------------------------------------------
void Lexer::handleFloatSuffix()
// Handle potential float suffix
{
    char c = peek();
    if ((c == 'f') || (c == 'F') || (c == 'l') || (c == 'L')) consume();
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleBinaryLiteral()
// Handle a literal starting with 0b
{
    char c = peek();
    if ((c < '0') || (c > '1')) {
        addError(tokenStart, "binary literal cannot be empty (0B must be followed by binary digits)");
        return Token::Error;
    }
    while (true) {
        if (loc.byteOfs >= input.length()) break;
        char c = input[loc.byteOfs];
        if ((c != '\'') && ((c < '0') || (c > '1'))) break;
        consume();
    }
    handleIntegerSuffix();
    return Token::BinaryLiteral;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleHexLiteral()
// Handle a literal starting with 0x
{
    static constinit auto isHexDigit = [](char c) { return ((c >= '0') && (c <= '9')) || ((c >= 'A') && (c <= 'F')) || ((c >= 'a') && (c <= 'f')); };

    auto tryConsumeExponent = [this]() {
        char c1 = peek(0), c2 = peek(1);
        if (((c1 == 'p') || (c1 == 'P')) && ((c2 == '+') || (c2 == '-') || ((c2 >= '0') && (c2 <= '9')))) {
            consume();
            if (char c = peek(); (c == '+') || (c == '-')) consume();
            while (true) {
                char c = peek();
                if ((!isHexDigit(c)) && (c != '\'')) break;
                consume();
            }
            return true;
        }
        return false;
    };

    char c = peek();
    if ((!isHexDigit(c)) && (c != '.')) {
        addError(tokenStart, "hexadecimal literal cannot be empty (0X must be followed by hexadecimal digits)");
        return Token::Error;
    }
    while (true) {
        if (loc.byteOfs >= input.length()) break;
        char c = input[loc.byteOfs];
        if (c == '.') {
            consume();
            while (true) {
                char c = peek();
                if (!(isHexDigit(c) || (c == '\''))) break;
                consume();
            }
            tryConsumeExponent();
            handleFloatSuffix();
            return Token::DecimalLiteral;
        }
        if ((c == 'p') || (c == 'P') && tryConsumeExponent()) {
            handleFloatSuffix();
            return Token::DecimalLiteral;
        }
        if ((c != '\'') && (!isHexDigit(c))) break;
        consume();
    }
    handleIntegerSuffix();
    return Token::HexadecimalLiteral;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleOctalLiteral()
// Handle a literal starting with 0[0-7]
{
    // No need to check for empty literals, we already know that we have at least two digits
    while (true) {
        if (loc.byteOfs >= input.length()) break;
        char c = input[loc.byteOfs];
        if ((c != '\'') && ((c < '0') || (c > '7'))) break;
        consume();
    }
    handleIntegerSuffix();
    return Token::OctalLiteral;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleDecimalLiteral()
// Handle a decimal literal
{
    auto consumeDigitSequence = [this]() {
        while (true) {
            char c = peek();
            if (((c < '0') || (c > '9')) && (c != '\'')) break;
            consume();
        }
    };
    auto tryConsumeExponent = [&]() {
        char c1 = peek(0), c2 = peek(1);
        if (((c1 == 'e') || (c1 == 'E')) && ((c2 == '+') || (c2 == '-') || ((c2 >= '0') && (c2 <= '9')))) {
            consume();
            if (char c = peek(); (c == '+') || (c == '-')) consume();
            consumeDigitSequence();
            return true;
        }
        return false;
    };

    // No need to check for empty literals, we already know that we have at least two digits
    while (true) {
        if (loc.byteOfs >= input.length()) break;
        char c = input[loc.byteOfs];
        if (c == '.') {
            consume();
            consumeDigitSequence();
            tryConsumeExponent();
            handleFloatSuffix();
            return Token::DecimalLiteral;
        }
        if (((c == 'E') || (c == 'e')) && tryConsumeExponent()) {
            handleFloatSuffix();
            return Token::DecimalLiteral;
        }
        if ((c != '\'') && ((c < '0') || (c > '9'))) break;
        consume();
    }
    handleIntegerSuffix();
    return Token::DecimalLiteral;
}
//---------------------------------------------------------------------------
unsigned Lexer::isStringPrefix()
// Check for string prefix
{
    char c1 = input[loc.byteOfs - 1];
    char c2 = peek(0), c3 = peek(1);
    if ((c1 == 'u') || (c1 == 'U')) {
        if (c2 == '8') {
            if ((c3 == '\'') || (c3 == '"')) return 2;
            if ((c3 == 'R') && (peek(2) == '"')) return 3;
        }
        if ((c2 == '\'') || (c2 == '"')) return 1;
        if ((c2 == 'R') && (c3 == '"')) return 2;
    }
    if ((c1 == 'R') && (c2 == '"')) return 2;

    return 0;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleStringLiteral(unsigned prefixLen)
// Handle a string literal
{
    // We handle " and ' with the same function, retrieve the quote
    char quote = input[loc.byteOfs + prefixLen - 1];
    consume(prefixLen);

    // A raw string?
    if ((quote == 'R') && (prefixLen > 0) && (input[loc.byteOfs + prefixLen - 1] == 'R')) {
        unsigned delimStart = loc.byteOfs;
        while ((loc.byteOfs < input.length()) && input[loc.byteOfs] != '(') processBlockChar();
        if (loc.byteOfs >= input.length()) {
            addError(tokenStart, "unterminated raw string");
            return Token::Error;
        }
        unsigned delimLen = loc.byteOfs - delimStart;
        string_view delim(input.data() + delimStart, delimLen);
        while (true) {
            if ((loc.byteOfs + 1 + delimLen) > input.length()) {
                addError(tokenStart, "unterminated raw string");
                return Token::Error;
            }
            char c = input[loc.byteOfs];
            processBlockChar();
            if ((c == ')') && (input[loc.byteOfs + delimLen] == '"') && (string_view(input.data() + loc.byteOfs, delimLen) == delim)) {
                for (unsigned index = 0; index != delimLen + 1; ++index) processBlockChar();
                return Token::StringLiteral;
            }
        }
    }

    // A regular string
    while (true) {
        if (loc.byteOfs >= input.length()) {
            addError(tokenStart, "unterminated character literal");
            return Token::Error;
        }
        char c = input[loc.byteOfs];
        processBlockChar();
        if (c == quote) break;
        if (c == '\\') {
            if (loc.byteOfs >= input.length()) {
                addError(tokenStart, "invalid escape sequence");
                return Token::Error;
            }
            processBlockChar();
        }
    }
    return Token::StringLiteral;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::handleIdentifier()
// Handle an identifier
{
    while (true) {
        if (loc.byteOfs >= input.length()) break;
        char c1 = input[loc.byteOfs];
        if (((c1 >= 'A') && (c1 <= 'Z')) || ((c1 >= 'a') && (c1 <= 'z')) || ((c1 >= '0') && (c1 <= '9')) || (c1 == '_')) {
            consume();
        } else
            break;
    }

    // Check the identifier for keywords
    string_view i(input.data() + tokenStart.byteOfs, input.data() + loc.byteOfs);
    if (auto iter = keywords.find(i); iter != keywords.end()) {
        switch (iter->second) {
            case Keyword::Union: addError(tokenStart, "unsafe 'union's are not supported in Cpp2 - use std::variant instead"); break;
            case Keyword::Delete: addError(tokenStart, "'delete' and owning raw pointers are not supported in Cpp2   - use unique.new<T>, shared.new<T>, or gc.new<T> instead (in that order)"); break;
            default: break;
        }
        return Token::Keyword;
    }

    // Check for alternative tokens
    if (auto iter = alternativeTokens.find(i); iter != alternativeTokens.end()) {
        switch (iter->second) {
            case AltToken::And: return Token::LogicalAnd;
            case AltToken::Bitor: return Token::Pipe;
            case AltToken::Or: return Token::LogicalOr;
            case AltToken::Xor: return Token::Carret;
            case AltToken::Compl: return Token::Tilde;
            case AltToken::Bitand: return Token::Ampersand;
            case AltToken::And_eq: return Token::AmpersandEq;
            case AltToken::Or_eq: return Token::PipeEq;
            case AltToken::Xor_eq: return Token::CarretEq;
            case AltToken::Not: return Token::Not;
            case AltToken::Not_eq: return Token::NotEqualComparison;
        }
    }

    // This here is ugly as it treats NULL like a keyword. But cppfront does the same
    if (i == "NULL"sv) {
        addError(tokenStart, "'NULL' is not supported in Cpp2 - for a local pointer variable, leave it uninitialized instead, and set it to a non-null value when you have one");
    }

    return Token::Identifier;
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::next()
// Read the next token
{
    unsigned limit = input.length();

    auto multiByte = [&] [[nodiscard]] (unsigned skip, Token tok) {
        consume(skip);
        return tok;
    };

    while (true) {
        if (loc.byteOfs >= limit) return Token::Eof;
        tokenStart = loc;
        ++loc.column;
        char c1 = input[loc.byteOfs++];
        char c2 = peek(0);
        char c3 = peek(1);

        switch (c1) {
            case ' ':
            case '\f':
            case '\v': continue;
            case '\t': {
                if ((loc.column - 1) % tabWidth) loc.column += tabWidth - ((loc.column - 1) % tabWidth);
                continue;
            }
            case '\n':
                if (c2 == '\r') loc.byteOfs++;
                loc.line++;
                loc.column = 1;
                continue;
            case '\r':
                if (c2 == '\n') loc.byteOfs++;
                loc.line++;
                loc.column = 1;
                continue;
            case '/':
                if (c2 == '/') {
                    handleSingleLineComment();
                    continue;
                }
                if (c2 == '*') {
                    handleMultiLineComment();
                    continue;
                }
                if (c2 == '=') return multiByte(1, Token::SlashEq);
                return Token::Slash;
            case '<':
                if (c2 == '<') return multiByte(1 + (c3 == '='), (c3 == '=') ? Token::LeftShiftEq : Token::LeftShift);
                if (c2 == '=') return multiByte(1 + (c3 == '>'), (c3 == '>') ? Token::Spaceship : Token::LessEq);
                return Token::Less;
            case '>':
                if (c2 == '=') return multiByte(1, Token::GreaterEq);
                return Token::Greater;
            case '+':
                if (c2 == '+') return multiByte(1, Token::PlusPlus);
                if (c2 == '=') return multiByte(1, Token::PlusEq);
                return Token::Plus;
            case '-':
                if (c2 == '-') return multiByte(1, Token::MinusMinus);
                if (c2 == '=') return multiByte(1, Token::MinusEq);
                return Token::Minus;
            case '|':
                if (c2 == '|') return multiByte(1 + (c3 == '='), (c3 == '=') ? Token::LogicalOrEq : Token::LogicalOr);
                if (c2 == '=') return multiByte(1, Token::PipeEq);
                return Token::Pipe;
            case '&':
                if (c2 == '&') return multiByte(1 + (c3 == '='), (c3 == '=') ? Token::LogicalAndEq : Token::LogicalAnd);
                if (c2 == '=') return multiByte(1, Token::AmpersandEq);
                return Token::Ampersand;
            case '*':
                if (c2 == '=') return multiByte(1, Token::MultiplyEq);
                return Token::Multiply;
            case '%':
                if (c2 == '=') return multiByte(1, Token::ModuloEq);
                return Token::Modulo;
            case '^':
                if (c2 == '=') return multiByte(1, Token::CarretEq);
                return Token::Carret;
            case '~':
                if (c2 == '=') return multiByte(1, Token::TildeEq);
                return Token::Tilde;
            case '=':
                if (c2 == '=') return multiByte(1, Token::EqualComparison);
                return Token::Assignment;
            case '!':
                if (c2 == '=') return multiByte(1, Token::NotEqualComparison);
                return Token::Not;
            case '.':
                if ((c2 == '.') && (c3 == '.')) return multiByte(2, Token::Ellipsis);
                if ((c2 >= '0') && (c2 <= '9') || (c2 == '\'')) {
                    --loc.byteOfs;
                    --loc.column;
                    return handleDecimalLiteral();
                }
                return Token::Dot;
            case ':':
                if (c2 == ':') return multiByte(2, Token::Scope);
                return Token::Colon;
            case '{': return Token::LeftBrace;
            case '}': return Token::RightBrace;
            case '(': return Token::LeftParen;
            case ')': return Token::RightParen;
            case '[': return Token::LeftBracket;
            case ']': return Token::RightBracket;
            case ';': return Token::Semicolon;
            case ',': return Token::Comma;
            case '?': return Token::QuestionMark;
            case '$': return Token::Dollar;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                if (c1 == '0') {
                    if ((c2 == 'b') || (c2 == 'B')) return handleBinaryLiteral();
                    if ((c2 == 'x') || (c2 == 'X')) return handleHexLiteral();
                    if ((c2 >= '0') && (c2 <= '7')) return handleOctalLiteral();
                }
                return handleDecimalLiteral();
            case '\'': return handleStringLiteral(0);
            case '"': return handleStringLiteral(0);
            case '#':
                while ((loc.byteOfs < input.length()) && (input[loc.byteOfs] != '\n') && (input[loc.byteOfs] != '\r')) consume();
                addError(tokenStart, "preprocessor statements not supported in cpp2 mode");
                return Token::Error;
            default:
                if (isStringPrefix()) handleStringLiteral(isStringPrefix());

                if (((c1 >= 'A') && (c1 <= 'Z')) || ((c1 >= 'a') && (c1 <= 'z')) || (c1 == '_'))
                    return handleIdentifier();
                addError(tokenStart, string("unexpected text ") + c1);
                return Token::Error;
        }
    }
}
//---------------------------------------------------------------------------
Lexer::Token Lexer::next(TokenInfo& info)
// Collect the token
{
    Token token = next();
    info.content = string_view(input.data() + tokenStart.byteOfs, input.data() + loc.byteOfs);
    if (token == Token::Keyword) info.keyword = keywords.find(info.content)->second;
    info.fromLine = tokenStart.line;
    info.fromColumn = tokenStart.column;
    info.toLine = loc.line;
    info.toColumn = loc.column;
    return token;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
