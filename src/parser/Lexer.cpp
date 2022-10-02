#include "Lexer.hpp"
#include <fstream>
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
static const unordered_map<string_view, Lexer::Keyword> keywords = {
    {"alignas", Lexer::Keyword::Alignas}, {"alignof", Lexer::Keyword::Alignof}, {"asm", Lexer::Keyword::Asm}, {"as", Lexer::Keyword::As}, {"auto", Lexer::Keyword::Auto}, {"bool", Lexer::Keyword::Bool}, {"break", Lexer::Keyword::Break}, {"case", Lexer::Keyword::Case}, {"catch", Lexer::Keyword::Catch}, {"char", Lexer::Keyword::Char}, {"char16_t", Lexer::Keyword::Char16_t}, {"Char32_t", Lexer::Keyword::Char32_t}, {"char8_t", Lexer::Keyword::Char8_t}, {"class", Lexer::Keyword::Class}, {"co_await", Lexer::Keyword::Co_await}, {"co_return", Lexer::Keyword::Co_return}, {"co_yield", Lexer::Keyword::Co_yield}, {"concept", Lexer::Keyword::Concept}, {"const", Lexer::Keyword::Const}, {"const_cast", Lexer::Keyword::Const_cast}, {"consteval", Lexer::Keyword::Consteval}, {"constinit", Lexer::Keyword::Constinit}, {"continue", Lexer::Keyword::Continue}, {"decltype", Lexer::Keyword::Decltype}, {"default", Lexer::Keyword::Default}, {"double", Lexer::Keyword::Double}, {"do", Lexer::Keyword::Do}, {"dynamic_cast", Lexer::Keyword::Dynamic_cast}, {"else", Lexer::Keyword::Else}, {"enum", Lexer::Keyword::Enum}, {"explicit", Lexer::Keyword::Explicit}, {"export", Lexer::Keyword::Export}, {"extern", Lexer::Keyword::Extern}, {"false", Lexer::Keyword::False}, {"float", Lexer::Keyword::Float}, {"for", Lexer::Keyword::For}, {"friend", Lexer::Keyword::Friend}, {"goto", Lexer::Keyword::Goto}, {"if", Lexer::Keyword::If}, {"import", Lexer::Keyword::Import}, {"inline", Lexer::Keyword::Inline}, {"int", Lexer::Keyword::Int}, {"is", Lexer::Keyword::Is}, {"long", Lexer::Keyword::Long}, {"module", Lexer::Keyword::Module}, {"mutable", Lexer::Keyword::Mutable}, {"namespace", Lexer::Keyword::Namespace}, {"new", Lexer::Keyword::New}, {"noexcept", Lexer::Keyword::Noexcept}, {"nullptr", Lexer::Keyword::Nullptr}, {"operator", Lexer::Keyword::Operator}, {"private", Lexer::Keyword::Private}, {"protected", Lexer::Keyword::Protected}, {"public", Lexer::Keyword::Public}, {"register", Lexer::Keyword::Register}, {"reinterpret_cast", Lexer::Keyword::Reinterpret_cast}, {"requires", Lexer::Keyword::Requires}, {"return", Lexer::Keyword::Return}, {"short", Lexer::Keyword::Short}, {"signed", Lexer::Keyword::Signed}, {"sizeof", Lexer::Keyword::Sizeof}, {"static", Lexer::Keyword::Static}, {"static_assert", Lexer::Keyword::Static_assert}, {"static_cast", Lexer::Keyword::Static_cast}, {"struct", Lexer::Keyword::Struct}, {"switch", Lexer::Keyword::Switch}, {"template", Lexer::Keyword::Template}, {"this", Lexer::Keyword::This}, {"thread_local", Lexer::Keyword::Thread_local}, {"throws", Lexer::Keyword::Throws}, {"throw", Lexer::Keyword::Throw}, {"true", Lexer::Keyword::True}, {"try", Lexer::Keyword::Try}, {"typedef", Lexer::Keyword::Typedef}, {"typeid", Lexer::Keyword::Typeid}, {"typename", Lexer::Keyword::Typename}, {"unsigned", Lexer::Keyword::Unsigned}, {"using", Lexer::Keyword::Using}, {"virtual", Lexer::Keyword::Virtual}, {"void", Lexer::Keyword::Void}, {"volatile", Lexer::Keyword::Volatile}, {"wchar_t", Lexer::Keyword::Wchar_t}, {"while", Lexer::Keyword::While}};
//---------------------------------------------------------------------------
Lexer::Lexer()
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
bool Lexer::loadFile(const string& fileName)
// Load the input from a file, reset the lexer
{
    ifstream in(fileName);
    if (!in.is_open()) {
        addError({}, "unable top open " + fileName);
        return false;
    }
    input = string(istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
    loc = {1, 1, 0};
    return true;
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
            ;
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
        if (loc.byteOfs >= input.length())
            ;
        break;
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
        if (loc.byteOfs >= input.length())
            ;
        break;
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
        if (loc.byteOfs >= input.length())
            ;
        break;
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
        if (loc.byteOfs >= input.length())
            ;
        break;
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
    if (auto iter = keywords.find(i); iter != keywords.end())
        return Token::Keyword;
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
            default:
                if (isStringPrefix()) handleStringLiteral(isStringPrefix());

                if (((c1 >= 'A') && (c1 <= 'Z')) || ((c1 >= 'a') && (c1 <= 'z')) || (c1 == '_'))
                    return handleIdentifier();
                addError(tokenStart, string("unexpected_text ") + c1);
                return Token::Error;
        }
    }
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
