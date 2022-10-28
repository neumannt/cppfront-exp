#include "semana/SemanticAnalysis.hpp"
#include "parser/AST.hpp"
#include "parser/Parser.hpp"
#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
#include "program/Module.hpp"
#include "program/Namespace.hpp"
#include "program/Program.hpp"
#include "program/Statement.hpp"
#include "program/Type.hpp"
#include "semana/Scope.hpp"
#include "stdlib/Stdlib.hpp"
#include <cassert>
#include <limits>
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace {
//---------------------------------------------------------------------------
/// Helper to iterate over an AST list with unspecific element types
class ASTList {
    private:
    /// The list
    const AST* list;

    public:
    /// Constructor
    explicit ASTList(const AST* list) : list(list) {}

    /// An iterator
    class iterator {
        private:
        /// The position
        const AST* pos;

        public:
        /// Constructor
        explicit iterator(const AST* pos) : pos(pos) {}

        /// Comparison
        bool operator==(iterator other) const { return pos == other.pos; }
        /// Comparison
        bool operator!=(iterator other) const { return pos != other.pos; }

        /// Deref
        const AST* operator*() const { return pos->getAny(0); }
        /// Advance
        iterator& operator++() {
            pos = pos->getOrNull(1, pos->getType());
            return *this;
        }
    };
    /// Iterator begin
    iterator begin() const { return iterator(list); }
    /// Iterator end
    iterator end() const { return iterator(nullptr); }
};
//---------------------------------------------------------------------------
/// Helper to iterate over an AST list with a given element types
template <AST::Type elementType>
class List {
    private:
    /// The list
    const AST* list;

    public:
    /// Constructor
    explicit List(const AST* list) : list(list) {}

    /// An iterator
    class iterator {
        private:
        /// The position
        const AST* pos;

        public:
        /// Constructor
        explicit iterator(const AST* pos) : pos(pos) {}

        /// Comparison
        bool operator==(iterator other) const { return pos == other.pos; }
        /// Comparison
        bool operator!=(iterator other) const { return pos != other.pos; }

        /// Deref
        const AST* operator*() const { return pos->get(0, elementType); }
        /// Advance
        iterator& operator++() {
            pos = pos->getOrNull(1, pos->getType());
            return *this;
        }
    };
    /// Iterator begin
    iterator begin() const { return iterator(list); }
    /// Iterator end
    iterator end() const { return iterator(nullptr); }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
SemanticAnalysis::SemanticAnalysis(string_view fileName, string_view content)
    : mapping(fileName, content), program(make_unique<Program>()), target(make_unique<Module>())
// Constructor
{
}
//---------------------------------------------------------------------------
SemanticAnalysis::~SemanticAnalysis()
// Destructor
{
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::loadStdlib()
// Load the C++1 stdlib mock
{
    if (stdlibMock) return false;
    stdlibMock = make_unique<Parser>();
    auto ast = stdlibMock->parseString("<stdlib>", Stdlib::interface);
    if (!ast) return false;
    RangeMapping mp(stdlibMock->getFileName(), stdlibMock->getContent());
    swap(mapping, mp);
    inStdlib = true;
    if (!analyze(ast)) return false;
    inStdlib = false;
    swap(mapping, mp);
    return true;
}
//---------------------------------------------------------------------------
void SemanticAnalysis::addError(SourceLocation loc, string text)
// Store an error message
{
    errors.emplace_back(loc, move(text));
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::addError(const AST* loc, std::string text)
// Store an error message
{
    addError(mapping.getBegin(loc->getRange()), move(text));
    return false;
}
//---------------------------------------------------------------------------
string_view SemanticAnalysis::accessText(const AST* ast)
// Access the content of a node
{
    auto range = ast->getRange();
    return mapping.getContent().substr(range.from, range.to - range.from);
}
//---------------------------------------------------------------------------
string_view SemanticAnalysis::extractIdentifier(const AST* ast)
// Extract an identifier
{
    assert(ast->getType() == AST::Identifier);
    return accessText(ast);
}
//---------------------------------------------------------------------------
DeclarationId SemanticAnalysis::extractDeclarationId(const AST* ast)
// Extract a function if
{
    if (ast->getType() == AST::OperatorName) {
        switch (ast->getSubType<ast::OperatorName>()) {
            case ast::OperatorName::BitAnd: return DeclarationId(DeclarationId::Category::OperatorBitAnd);
            case ast::OperatorName::BitAndEq: return DeclarationId(DeclarationId::Category::OperatorBitAndEq);
            case ast::OperatorName::BitOr: return DeclarationId(DeclarationId::Category::OperatorBitOr);
            case ast::OperatorName::BitOrEq: return DeclarationId(DeclarationId::Category::OperatorBitOrEq);
            case ast::OperatorName::BitXor: return DeclarationId(DeclarationId::Category::OperatorBitXor);
            case ast::OperatorName::BitXorEq: return DeclarationId(DeclarationId::Category::OperatorBitXorEq);
            case ast::OperatorName::Complement: return DeclarationId(DeclarationId::Category::OperatorComplement);
            case ast::OperatorName::Div: return DeclarationId(DeclarationId::Category::OperatorDiv);
            case ast::OperatorName::DivEq: return DeclarationId(DeclarationId::Category::OperatorDivEq);
            case ast::OperatorName::Equal: return DeclarationId(DeclarationId::Category::OperatorEqual);
            case ast::OperatorName::Greater: return DeclarationId(DeclarationId::Category::OperatorGreater);
            case ast::OperatorName::GreaterEq: return DeclarationId(DeclarationId::Category::OperatorGreaterEq);
            case ast::OperatorName::LeftShift: return DeclarationId(DeclarationId::Category::OperatorLeftShift);
            case ast::OperatorName::LeftShiftEq: return DeclarationId(DeclarationId::Category::OperatorLeftShiftEq);
            case ast::OperatorName::Less: return DeclarationId(DeclarationId::Category::OperatorLess);
            case ast::OperatorName::LessEq: return DeclarationId(DeclarationId::Category::OperatorLessEq);
            case ast::OperatorName::LogicalAnd: return DeclarationId(DeclarationId::Category::OperatorAnd);
            case ast::OperatorName::LogicalOr: return DeclarationId(DeclarationId::Category::OperatorOr);
            case ast::OperatorName::Minus: return DeclarationId(DeclarationId::Category::OperatorMinus);
            case ast::OperatorName::MinusEq: return DeclarationId(DeclarationId::Category::OperatorMinusEq);
            case ast::OperatorName::MinusMinus: return DeclarationId(DeclarationId::Category::OperatorMinusMinus);
            case ast::OperatorName::Modulo: return DeclarationId(DeclarationId::Category::OperatorModulo);
            case ast::OperatorName::ModuloEq: return DeclarationId(DeclarationId::Category::OperatorModuloEq);
            case ast::OperatorName::Mul: return DeclarationId(DeclarationId::Category::OperatorMul);
            case ast::OperatorName::MulEq: return DeclarationId(DeclarationId::Category::OperatorMulEq);
            case ast::OperatorName::Not: return DeclarationId(DeclarationId::Category::OperatorNot);
            case ast::OperatorName::NotEqual: return DeclarationId(DeclarationId::Category::OperatorNotEqual);
            case ast::OperatorName::Plus: return DeclarationId(DeclarationId::Category::OperatorPlus);
            case ast::OperatorName::PlusEq: return DeclarationId(DeclarationId::Category::OperatorPlusEq);
            case ast::OperatorName::PlusPlus: return DeclarationId(DeclarationId::Category::OperatorPlusPlus);
            case ast::OperatorName::RightShift: return DeclarationId(DeclarationId::Category::OperatorRightShift);
            case ast::OperatorName::RightShiftEq: return DeclarationId(DeclarationId::Category::OperatorRightShiftEq);
            case ast::OperatorName::Spaceship: return DeclarationId(DeclarationId::Category::OperatorSpaceship);
        }
    }
    return DeclarationId(string(extractIdentifier(ast)));
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::enforceConvertible(const AST* loc, std::unique_ptr<Expression>& exp, const Type* target, [[maybe_unused]] bool explicitScope)
// Make sure an expression is convertible into a certain type
{
    auto ta = exp->getType()->getEffectiveType();
    auto tb = target->getEffectiveType();

    // Trivial conversion?
    if (ta == tb) return true;

    // Standard conversions
    if (tb->isPointerType()) {
        if (ta->isFundamentalType() && (tb->as<FundamentalType>()->getId() == Type::FundamentalTypeId::NullptrType))
            return true;
        // TODO handle const
    } else if (tb->isFundamentalType()) {
        auto ib = tb->as<FundamentalType>()->getId();
        if (ib == Type::FundamentalTypeId::Bool) {
            if (ta->isPointerType()) return true;
        }
        if (ta->isFundamentalType()) {
            auto ia = ta->as<FundamentalType>()->getId();
            // So far all conversions are valid as long as both are not void
            if ((ia != Type::FundamentalTypeId::Void) && (ib != Type::FundamentalTypeId::Void) && (ia != Type::FundamentalTypeId::NullptrType) && (ib != Type::FundamentalTypeId::NullptrType))
                return true;
        }
    }

    // TODO Check user defined conversions
    addError(loc, "no type conversion found");
    return false;
}
//---------------------------------------------------------------------------
const Type* SemanticAnalysis::resolveOperator([[maybe_unused]] Scope& scope, [[maybe_unused]] const DeclarationId& id, [[maybe_unused]] const Expression& left, [[maybe_unused]] const Expression& right)
// Try to resolve an operator
{
    return nullptr; // TODO
}
//---------------------------------------------------------------------------
Declaration* SemanticAnalysis::resolveUnqualifiedId(Scope& scope, const AST* ast)
// Resolve an unqualified id
{
    auto subType = ast->getSubType<ast::UnqualifiedId>();
    if (subType == ast::UnqualifiedId::Template) {
        addError(ast, "templates not implemented yet"); // TODO
        return nullptr;
    }
    auto name = DeclarationId(extractIdentifier(ast->get(0, AST::Identifier)));
    auto ns = scope.getCurrentNamespace();
    while (ns) {
        auto d = ns->findDeclaration(name);
        if (d) return d;
        ns = ns->getParent();
    }
    addError(ast, "unknown type name '" + name.name + "'");
    return nullptr;
}
//---------------------------------------------------------------------------
Declaration* SemanticAnalysis::resolveQualifiedId(Scope& scope, const AST* ast)
// Resolve a qualified id
{
    if (ast->getSubType<ast::QualifiedId>() != ast::QualifiedId::Nested) {
        addError(ast, "type name expected");
        return nullptr;
    }

    // Interpret absolute path names
    auto nast = ast->get(0, AST::NestedNameSpecifier);
    Namespace* ns = scope.getCurrentNamespace();
    if (nast->getSubType<ast::NestedNameSpecifier>() == ast::NestedNameSpecifier::Absolute)
        ns = target->getGlobalNamespace();

    // Collect all parts
    vector<const AST*> parts;
    for (auto e : ASTList(nast->getAnyOrNull(0))) parts.push_back(e);

    // And resolve the path
    for (auto e : parts) {
        auto subType = e->getSubType<ast::UnqualifiedId>();
        if (subType == ast::UnqualifiedId::Template) {
            addError(e, "templates not implemented yet"); // TODO
            return nullptr;
        }
        auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

        auto d = ns->findDeclaration(name);
        if (!d) {
            addError(e, "'" + name.name + "' not found in '" + ns->getName() + "'");
            return nullptr;
        }
        Namespace* next;
        if (auto n = dynamic_cast<NamespaceDeclaration*>(d)) {
            next = n->getNamespace();
        } else if (auto c = dynamic_cast<ClassDeclaration*>(d)) {
            next = c->getClass();
        }
        if (!next) {
            addError(e, "'" + name.name + "' is not a namespace");
            return nullptr;
        }
        ns = next;
    }

    // Lookup the element itself
    auto e = ast->get(1, AST::UnqualifiedId);
    auto subType = e->getSubType<ast::UnqualifiedId>();
    if (subType == ast::UnqualifiedId::Template) {
        addError(ast, "templates not implemented yet"); // TODO
        return nullptr;
    }
    auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

    auto d = ns->findDeclaration(name);
    if (!d) {
        addError(e, "'" + name.name + "' not found in '" + ns->getName() + "'");
        return nullptr;
    }
    return d;
}
//---------------------------------------------------------------------------
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpression(Scope& scope, const AST* ast, const Type* typeHint)
// Analyze an expression
{
    switch (ast->getType()) {
        case AST::ExpressionListExpression: return analyzeExpressionListExpression(scope, ast, typeHint);
        case AST::AssignmentExpression: addError(ast, "assignment_expression not implemented yet"); return {}; // TODO
        case AST::BinaryExpression: return analyzeBinaryExpression(scope, ast);
        case AST::PrefixExpression: addError(ast, "prefix_expression not implemented yet"); return {}; // TODO
        case AST::PostfixExpression: addError(ast, "postfix_expression not implemented yet"); return {}; // TODO
        case AST::BracketExpression: addError(ast, "bracket_expression not implemented yet"); return {}; // TODO
        case AST::ParenExpression: addError(ast, "paren_expression not implemented yet"); return {}; // TODO
        case AST::DotExpression: addError(ast, "dot_expression not implemented yet"); return {}; // TODO
        case AST::InspectExpression: addError(ast, "inspect_expression not implemented yet"); return {}; // TODO
        case AST::Literal: return analyzeLiteral(ast);
        case AST::UnqualifiedId:
        case AST::QualifiedId: return analyzeIdExpressionExpression(scope, ast);
        case AST::UnnamedDeclaration: addError(ast, "lambda expressions not implemented yet"); return {}; // TODO
        case AST::NewExpression: addError(ast, "new expressions not implemented yet"); return {}; // TODO
        default: addError(ast, "invalid AST"); return {};
    }
}
//---------------------------------------------------------------------------
unique_ptr<Expression> SemanticAnalysis::analyzeLiteral(const AST* ast)
// Analyze a literal expression
{
    auto text = accessText(ast);
    auto loc = mapping.getBegin(ast->getRange());

    // Logic for handling integer formats
    auto handleInteger = [&](uint64_t v, string_view suffix, bool nonDecimal) -> unique_ptr<Expression> {
        // Interpret the suffix
        bool isUnsigned = false, hasZ = false;
        unsigned l = 0;
        for (char c : suffix) {
            switch (c) {
                case 'u':
                case 'U': isUnsigned = true; break;
                case 'l':
                case 'L': l++; break;
                case 'z':
                case 'Z': hasZ = true; break;
            }
        }

        // Choose the appropriate integer type
        uint64_t limit;
        const Type* type;
        using FundamentalTypeId = Type::FundamentalTypeId;
        auto consider = [&](uint64_t candidateLimit, FundamentalTypeId id, bool valid = true) -> bool {
            if (valid && (v <= candidateLimit)) {
                limit = candidateLimit;
                type = Type::getFundamentalType(*program, id);
                return true;
            }
            return false;
        };
        auto force = [&](uint64_t candidateLimit, FundamentalTypeId id) {
            limit = candidateLimit;
            type = Type::getFundamentalType(*program, id);
        };
        if (hasZ) {
            if (isUnsigned || (nonDecimal && (v > static_cast<uint64_t>(numeric_limits<long>::max())))) {
                force(numeric_limits<unsigned long>::max(), FundamentalTypeId::UnsignedLong);
            } else {
                force(numeric_limits<long>::max(), FundamentalTypeId::Long);
            }
        } else if (isUnsigned) {
            if (!consider(numeric_limits<unsigned>::max(), FundamentalTypeId::UnsignedInt, l < 1)) {
                if (!consider(numeric_limits<unsigned long>::max(), FundamentalTypeId::UnsignedLong, l < 2)) {
                    force(numeric_limits<unsigned long long>::max(), FundamentalTypeId::UnsignedLongLong);
                }
            }
        } else if (nonDecimal) {
            if (!consider(numeric_limits<int>::max(), FundamentalTypeId::Int, l < 1)) {
                if (!consider(numeric_limits<unsigned>::max(), FundamentalTypeId::UnsignedInt, l < 1)) {
                    if (!consider(numeric_limits<long>::max(), FundamentalTypeId::Long, l < 2)) {
                        if (!consider(numeric_limits<unsigned long>::max(), FundamentalTypeId::UnsignedLong, l < 2)) {
                            if (!consider(numeric_limits<long long>::max(), FundamentalTypeId::LongLong)) {
                                force(numeric_limits<unsigned long long>::max(), FundamentalTypeId::UnsignedLongLong);
                            }
                        }
                    }
                }
            }
        } else {
            if (!consider(numeric_limits<int>::max(), FundamentalTypeId::Int, l < 1)) {
                if (!consider(numeric_limits<long>::max(), FundamentalTypeId::Long, l < 2)) {
                    force(numeric_limits<long long>::max(), FundamentalTypeId::LongLong);
                }
            }
        }
        if (v > limit) {
            addError(ast, "integer value out of range");
            return {};
        }
        return make_unique<Literal>(loc, type, text);
    };

    // Interpret the literal
    switch (ast->getSubType<ast::Literal>()) {
        case ast::Literal::True: return make_unique<Literal>(loc, Type::getBool(*program), text);
        case ast::Literal::False: return make_unique<Literal>(loc, Type::getBool(*program), text);
        case ast::Literal::Nullptr: return make_unique<Literal>(loc, Type::getNullptrType(*program), text);
        case ast::Literal::CharLiteral: return make_unique<Literal>(loc, Type::getChar(*program), text);
        case ast::Literal::StringLiteral: return make_unique<Literal>(loc, Type::getChar(*program)->getPointerTo(), text);
        case ast::Literal::DecimalInteger: {
            uint64_t v = 0;
            unsigned index = 0;
            for (; index < text.length(); ++index) {
                char c = text[index];
                if (c == '\'') continue;
                if ((c < '0') || (c > '9')) break;
                uint64_t v1 = 10 * v, v2 = v1 + (c - '0');
                if ((v1 < v) || (v2 < v1)) {
                    addError(ast, "integer value out of range");
                    return {};
                }
                v = v2;
            }
            return handleInteger(v, text.substr(index), false);
        }
        case ast::Literal::OctalInteger: {
            uint64_t v = 0;
            unsigned index = 0;
            for (; index < text.length(); ++index) {
                char c = text[index];
                if (c == '\'') continue;
                if ((c < '0') || (c > '7')) break;
                uint64_t v1 = 8 * v, v2 = v1 + (c - '0');
                if ((v1 < v) || (v2 < v1)) {
                    addError(ast, "integer value out of range");
                    return {};
                }
                v = v2;
            }
            return handleInteger(v, text.substr(index), true);
        }
        case ast::Literal::HexInteger: {
            uint64_t v = 0;
            unsigned index = 0;
            for (; index < text.length(); ++index) {
                char c = text[index];
                if (c == '\'') continue;
                if (!(((c >= '0') && (c <= '9')) || ((c >= 'A') && (c <= 'F')) || ((c >= 'a') && (c <= 'f')))) break;
                unsigned n = ((c >= 'A') && (c <= 'F')) ? (c - 'A' + 10) : (((c >= 'a') && (c <= 'f')) ? (c - 'a' + 10) : (c - '0'));
                uint64_t v1 = 16 * v, v2 = v1 + n;
                if ((v1 < v) || (v2 < v1)) {
                    addError(ast, "integer value out of range");
                    return {};
                }
                v = v2;
            }
            return handleInteger(v, text.substr(index), true);
        }
        case ast::Literal::DecimalFloat:
        case ast::Literal::HexFloat:
            // Interpret type suffix if any
            if ((text.back() == 'l') || (text.back() == 'L')) return make_unique<Literal>(loc, Type::getLongDouble(*program), text);
            if ((text.back() == 'f') || (text.back() == 'F')) return make_unique<Literal>(loc, Type::getFloat(*program), text);
            return make_unique<Literal>(loc, Type::getDouble(*program), text);
    }
    return {};
}
//---------------------------------------------------------------------------
unique_ptr<Expression> SemanticAnalysis::analyzeBinaryExpression(Scope& scope, const AST* ast)
// Analyze a binary expression
{
    // Interpret the AST node type
    auto loc = mapping.getBegin(ast->getRange());
    auto subType = ast->getSubType<ast::BinaryExpression>();
    BinaryExpression::Op op;
    DeclarationId::Category id;
    bool isCmp = false;
    switch (subType) {
        case ast::BinaryExpression::LogicalAnd:
            op = BinaryExpression::LogicalAnd;
            id = DeclarationId::OperatorAnd;
            break;
        case ast::BinaryExpression::LogicalOr:
            op = BinaryExpression::LogicalOr;
            id = DeclarationId::OperatorOr;
            break;
        case ast::BinaryExpression::BitAnd:
            op = BinaryExpression::BitAnd;
            id = DeclarationId::OperatorBitAnd;
            break;
        case ast::BinaryExpression::BitOr:
            op = BinaryExpression::BitOr;
            id = DeclarationId::OperatorBitOr;
            break;
        case ast::BinaryExpression::BitXor:
            op = BinaryExpression::BitXor;
            id = DeclarationId::OperatorBitXor;
            break;
        case ast::BinaryExpression::Equal:
            op = BinaryExpression::Equal;
            id = DeclarationId::OperatorEqual;
            isCmp = true;
            break;
        case ast::BinaryExpression::NotEqual:
            op = BinaryExpression::NotEqual;
            id = DeclarationId::OperatorNotEqual;
            isCmp = true;
            break;
        case ast::BinaryExpression::Less:
            op = BinaryExpression::Less;
            id = DeclarationId::OperatorLess;
            isCmp = true;
            break;
        case ast::BinaryExpression::LessEq:
            op = BinaryExpression::LessEq;
            id = DeclarationId::OperatorLessEq;
            isCmp = true;
            break;
        case ast::BinaryExpression::Greater:
            op = BinaryExpression::Greater;
            id = DeclarationId::OperatorGreater;
            isCmp = true;
            break;
        case ast::BinaryExpression::GreaterEq:
            op = BinaryExpression::GreaterEq;
            id = DeclarationId::OperatorGreaterEq;
            isCmp = true;
            break;
        case ast::BinaryExpression::Spaceship:
            op = BinaryExpression::Spaceship;
            id = DeclarationId::OperatorSpaceship;
            break;
        case ast::BinaryExpression::LeftShift:
            op = BinaryExpression::LeftShift;
            id = DeclarationId::OperatorLeftShift;
            break;
        case ast::BinaryExpression::RightShift:
            op = BinaryExpression::RightShift;
            id = DeclarationId::OperatorRightShift;
            break;
        case ast::BinaryExpression::Plus:
            op = BinaryExpression::Plus;
            id = DeclarationId::OperatorPlus;
            break;
        case ast::BinaryExpression::Minus:
            op = BinaryExpression::Minus;
            id = DeclarationId::OperatorMinus;
            break;
        case ast::BinaryExpression::Mul:
            op = BinaryExpression::Mul;
            id = DeclarationId::OperatorMul;
            break;
        case ast::BinaryExpression::Div:
            op = BinaryExpression::Div;
            id = DeclarationId::OperatorDiv;
            break;
        case ast::BinaryExpression::Modulo:
            op = BinaryExpression::Modulo;
            id = DeclarationId::OperatorModulo;
            break;
        case ast::BinaryExpression::Is: addError(ast, "is not implemented yet"); return {}; // TODO
        case ast::BinaryExpression::As: addError(ast, "as not implemented yet"); return {}; // TODO
    }

    // Check the input
    auto left = analyzeExpression(scope, ast->getAny(0));
    if (!left) return {};
    auto right = analyzeExpression(scope, ast->getAny(1));
    if (!right) return {};

    // Check for overloaded operators
    DeclarationId fid(id);
    const Type* match = resolveOperator(scope, fid, *left, *right);
    if (match) return make_unique<BinaryExpression>(loc, match, op, move(left), move(right));

    // In case of comparisons, try again with spaceship
    if (isCmp) {
        match = resolveOperator(scope, DeclarationId::OperatorSpaceship, *left, *right);
        if (match) return make_unique<BinaryExpression>(loc, Type::getBool(*program), op, move(left), move(right));
    }

    // The usual arithmetic conversions
    using FundamentalTypeId = Type::FundamentalTypeId;
    const Type* leftType = left->getType();
    const Type* rightType = right->getType();
    FundamentalTypeId leftId = FundamentalTypeId::Void, rightId = FundamentalTypeId::Void;
    if (leftType->isFundamentalType() && rightType->isFundamentalType()) {
        leftId = leftType->as<FundamentalType>()->getId();
        rightId = rightType->as<FundamentalType>()->getId();

        // Promote to integer
        if ((leftId >= FundamentalTypeId::Char) && (leftId <= FundamentalTypeId::Int)) leftId = FundamentalTypeId::Int;
        if ((rightId >= FundamentalTypeId::Char) && (rightId <= FundamentalTypeId::Int)) rightId = FundamentalTypeId::Int;

        // Promotion rules
        if ((leftId == FundamentalTypeId::LongDouble) || (rightId == FundamentalTypeId::LongDouble)) {
            leftId = rightId = FundamentalTypeId::LongDouble;
        } else if ((leftId == FundamentalTypeId::Double) || (rightId == FundamentalTypeId::Double)) {
            leftId = rightId = FundamentalTypeId::Double;
        } else if ((leftId == FundamentalTypeId::Float) || (rightId == FundamentalTypeId::Float)) {
            leftId = rightId = FundamentalTypeId::Float;
        } else if (Type::isInteger(leftId) && Type::isInteger(rightId)) {
            if (Type::isUnsignedInt(leftId) == Type::isUnsignedInt(rightId)) {
                leftId = rightId = max(leftId, rightId);
            } else if ((Type::isUnsignedInt(leftId)) && (leftId > rightId)) {
                rightId = leftId;
            } else if ((Type::isUnsignedInt(rightId)) && (rightId > leftId)) {
                leftId = rightId;
            } else {
                leftId = rightId = max(leftId, rightId);
            }
        }
        leftType = FundamentalType::getFundamentalType(*program, leftId);
        rightType = FundamentalType::getFundamentalType(*program, rightId);
    }

    // Handle builtin operators
    switch (op) {
        case BinaryExpression::LogicalAnd:
        case BinaryExpression::LogicalOr:
            if ((!enforceConvertible(ast, left, Type::getBool(*program))) || (!enforceConvertible(ast, right, Type::getBool(*program)))) return {};
            return make_unique<BinaryExpression>(loc, Type::getBool(*program), op, move(left), move(right));
        case BinaryExpression::BitAnd:
        case BinaryExpression::BitOr:
        case BinaryExpression::BitXor:
        case BinaryExpression::LeftShift:
            op = BinaryExpression::LeftShift;
            id = DeclarationId::OperatorLeftShift;
            break;
        case BinaryExpression::RightShift:
            op = BinaryExpression::RightShift;
            id = DeclarationId::OperatorRightShift;
            break;
            if ((leftType == rightType) && Type::isInteger(leftId)) return make_unique<BinaryExpression>(loc, leftType, op, move(left), move(right));
            break;
        case BinaryExpression::Equal:
        case BinaryExpression::NotEqual:
        case BinaryExpression::Less:
        case BinaryExpression::LessEq:
        case BinaryExpression::Greater:
        case BinaryExpression::GreaterEq:
        case BinaryExpression::Spaceship:
            if (((leftType == rightType) && leftType->isPointerType()) || ((Type::isNumerical(leftId) && Type::isNumerical(rightId)))) return make_unique<BinaryExpression>(loc, Type::getBool(*program), op, move(left), move(right));
            break;
        case BinaryExpression::Plus:
        case BinaryExpression::Minus:
            if (leftType->isPointerType() || rightType->isPointerType()) {
                addError(ast, "pointer arithmetic is illegal - use std::span");
                return {};
            }
            [[fallthrough]];
        case BinaryExpression::Mul:
        case BinaryExpression::Div:
        case BinaryExpression::Modulo:
            if ((leftType == rightType) && (Type::isNumerical(leftId))) return make_unique<BinaryExpression>(loc, leftType, op, move(left), move(right));
            break;
    }

    addError(ast, "binary operator not supported for data types");
    return {};
}
//---------------------------------------------------------------------------
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpressionListExpression(Scope& scope, const AST* ast, [[maybe_unused]] const Type* typeHint)
// Analyze an expression
{
    // Check if this is something else than a simple value
    if ((!ast->getAnyOrNull(0)) || (ast->getAnyOrNull(0)->getAnyOrNull(1))) {
        addError(ast, "implicit object construction not implemented yet"); // TODO
        return {};
    }

    return analyzeExpression(scope, ast->getAny(0)->getAny(0));
}
//---------------------------------------------------------------------------
unique_ptr<Expression> SemanticAnalysis::analyzeIdExpressionExpression(Scope& scope, const AST* ast)
// Analyze an id expression that is part of an expression
{
    auto loc = mapping.getBegin(ast->getRange());
    Namespace* ns = scope.getCurrentNamespace();
    DeclarationId localId(""sv);
    if (ast->getType() == AST::UnqualifiedId) {
        auto subType = ast->getSubType<ast::UnqualifiedId>();
        if (subType == ast::UnqualifiedId::Template) {
            addError(ast, "templates not implemented yet"); // TODO
            return {};
        }
        localId = DeclarationId(extractIdentifier(ast->get(0, AST::Identifier)));
    } else {
        if (ast->getSubType<ast::QualifiedId>() != ast::QualifiedId::Nested) {
            addError(ast, "type name expected");
            return {};
        }

        // Interpret absolute path names
        auto nast = ast->get(0, AST::NestedNameSpecifier);
        if (nast->getSubType<ast::NestedNameSpecifier>() == ast::NestedNameSpecifier::Absolute)
            ns = target->getGlobalNamespace();

        // Collect all parts
        vector<const AST*> parts;
        for (auto e : ASTList(nast->getAnyOrNull(0))) parts.push_back(e);

        // And resolve the path
        for (auto e : parts) {
            auto subType = e->getSubType<ast::UnqualifiedId>();
            if (subType == ast::UnqualifiedId::Template) {
                addError(e, "templates not implemented yet"); // TODO
                return nullptr;
            }
            auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

            auto d = ns->findDeclaration(name);
            if (!d) {
                addError(e, "'" + name.name + "' not found in '" + ns->getName() + "'");
                return nullptr;
            }
            Namespace* next;
            if (auto n = dynamic_cast<NamespaceDeclaration*>(d)) {
                next = n->getNamespace();
            } else if (auto c = dynamic_cast<ClassDeclaration*>(d)) {
                next = c->getClass();
            }
            if (!next) {
                addError(e, "'" + name.name + "' is not a namespace");
                return nullptr;
            }
            ns = next;
        }

        // The last part of the path
        auto e = ast->get(1, AST::UnqualifiedId);
        auto subType = e->getSubType<ast::UnqualifiedId>();
        if (subType == ast::UnqualifiedId::Template) {
            addError(ast, "templates not implemented yet"); // TODO
            return nullptr;
        }
        localId = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));
    }

    // Lookup the element itself
    auto d = ns->findDeclaration(localId);
    if (!d) {
        addError(ast, "'" + localId.name + "' not found in '" + ns->getName() + "'");
        return {};
    }
    if (d->getCategory() != Declaration::Category::Variable) {
        addError(ast, "'" + localId.name + "' is not a variable");
        return {};
    }
    auto v = static_cast<VariableDeclaration*>(d);
    return make_unique<VariableExpression>(loc, v->getType(), v);
}
//---------------------------------------------------------------------------
unique_ptr<Statement> SemanticAnalysis::analyzeCompoundStatement(Scope& scope, const AST* ast)
// Analyze a compound statement
{
    auto begin = mapping.getBegin(ast->getRange());
    Scope innerScope(scope);
    vector<unique_ptr<Statement>> statements;
    for (auto s : ASTList(ast->getAnyOrNull(0))) {
        statements.push_back(analyzeStatement(innerScope, s));
        if (!statements.back()) return {};
    }
    auto end = mapping.getEnd(ast->getRange());

    return make_unique<CompoundStatement>(begin, end, move(statements));
}
//---------------------------------------------------------------------------
unique_ptr<Statement> SemanticAnalysis::analyzeReturnStatement(Scope& scope, const AST* ast)
// Analyze a return statement
{
    auto fs = scope.getCurrentFunction();
    if (!fs) {
        addError(ast, "return statements are only allowed within functions");
        return {};
    }
    auto ft = fs->functionType;
    auto begin = mapping.getBegin(ast->getRange());
    unique_ptr<Expression> arg;
    if (ft->returnValues.empty()) {
        if (ast->getAny(0)) {
            addError(ast, "cannot return a value in function returning void");
            return {};
        }
    } else if ((ft->returnValues.size() == 1) && (ft->returnValues[0].first.empty())) {
        if (!ast->getAny(0)) {
            addError(ast, "cannot return a value in function returning void");
            return {};
        }
        arg = analyzeExpression(scope, ast->getAny(0), ft->returnValues[0].second);
        if ((!arg) || (!enforceConvertible(ast->getAny(0), arg, ft->returnValues[0].second))) return {};
    } else {
        // TODO check that return values have been assigned
        if (ast->getAny(0)) {
            addError(ast, "return values are passed via named in this function");
            return {};
        }
    }

    return make_unique<ReturnStatement>(begin, move(arg));
}
//---------------------------------------------------------------------------
unique_ptr<Statement> SemanticAnalysis::analyzeExpressionStatement(Scope& scope, const AST* ast)
// Analyze an expression statement
{
    auto begin = mapping.getBegin(ast->getRange());
    unique_ptr<Expression> arg = analyzeExpression(scope, ast->getAny(0));
    if (!arg) return {};

    return make_unique<ExpressionStatement>(begin, move(arg));
}
//---------------------------------------------------------------------------
std::unique_ptr<Statement> SemanticAnalysis::analyzeStatement(Scope& scope, const AST* ast)
// Analyze a statement
{
    switch (ast->getType()) {
        case AST::DeclarationStatement: addError(ast, "declaration_statement not implemented yet"); return {}; // TODO
        case AST::CompoundStatement: return analyzeCompoundStatement(scope, ast);
        case AST::ReturnStatement: return analyzeReturnStatement(scope, ast);
        case AST::SelectionStatement: addError(ast, "selection_statement not implemented yet"); return {}; // TODO
        case AST::WhileStatement: addError(ast, "while_statement not implemented yet"); return {}; // TODO
        case AST::DoWhileStatement: addError(ast, "do_while_statement not implemented yet"); return {}; // TODO
        case AST::ForStatement: addError(ast, "for_statement not implemented yet"); return {}; // TODO
        case AST::InspectExpression: addError(ast, "inspect_statment not implemented yet"); return {}; // TODO
        case AST::ExpressionStatement: return analyzeExpressionStatement(scope, ast);
        default: addError(ast, "invalid AST"); return {};
    }
}
//---------------------------------------------------------------------------
const Type* SemanticAnalysis::analyzeIdExpression(Scope& scope, const AST* ast)
// Analyze an id-expression with optional pointer markers
{
    // Handle const
    bool isConst = false;
    if ((ast->getType() == AST::TypeModifier) && (ast->getSubType<ast::TypeModifier>() == ast::TypeModifier::Const)) {
        isConst = true;
        ast = ast->getAny(0);
    }
    if ((ast->getType() == AST::TypeModifier) && (ast->getSubType<ast::TypeModifier>() == ast::TypeModifier::Const) && isConst) {
        addError(ast, "const qualifier most only appear once");
        return nullptr;
    }

    // Interpret the type
    switch (ast->getType()) {
        case AST::UnqualifiedId:
        case AST::QualifiedId: {
            auto decl = (ast->getType() == AST::UnqualifiedId) ? resolveUnqualifiedId(scope, ast) : resolveQualifiedId(scope, ast);
            if (!decl) return nullptr;
            if (!decl->isType()) {
                addError(ast, "invalid type expression");
                return nullptr;
            }
            return decl->getCorrespondingType();
        }
        case AST::FundamentalType: {
            switch (ast->getSubType<ast::FundamentalType>()) {
                case ast::FundamentalType::Void: return Type::getVoid(*program);
                case ast::FundamentalType::Char: {
                    // char can be modified by signed and unsigned
                    bool isSigned = false, isUnsigned = false;
                    for (auto m : List<AST::FundamentalTypeModifier>(ast->getOrNull(0, AST::FundamentalTypeModifierList))) {
                        if (isSigned || isUnsigned) {
                            addError(m, "invalid type modifier");
                            return nullptr;
                        }
                        switch (m->getSubType<ast::FundamentalTypeModifier>()) {
                            case ast::FundamentalTypeModifier::Signed: isSigned = true; break;
                            case ast::FundamentalTypeModifier::Unsigned: isUnsigned = true; break;
                            case ast::FundamentalTypeModifier::Long: addError(m, "'long' is not valid for char types"); return nullptr;
                            case ast::FundamentalTypeModifier::Short: addError(m, "'short' is not valid for char types"); return nullptr;
                        }
                    }
                    // by default we assume char is signed. Should we make this platform specific? Could also be handled when writing C++1 out
                    return isUnsigned ? Type::getUnsignedChar(*program) : Type::getChar(*program);
                }
                case ast::FundamentalType::Char8: return Type::getChar8(*program);
                case ast::FundamentalType::Char16: return Type::getChar16(*program);
                case ast::FundamentalType::Char32: return Type::getChar32(*program);
                case ast::FundamentalType::WChar: return Type::getWChar(*program);
                case ast::FundamentalType::Bool: return Type::getBool(*program);
                case ast::FundamentalType::Float: return Type::getFloat(*program);
                case ast::FundamentalType::Double: return Type::getDouble(*program);
                case ast::FundamentalType::LongDouble: return Type::getLongDouble(*program);
                case ast::FundamentalType::Int: {
                    // Int can be modified by short/long/long long/signed/unsigned, in any order
                    bool isSigned = false, isUnsigned = false;
                    bool isShort = false;
                    unsigned isLong = 0;
                    auto invalidModifier = [this](const AST* ast) {
                        addError(ast, "invalid type modifier");
                        return nullptr;
                    };
                    for (auto m : List<AST::FundamentalTypeModifier>(ast->getOrNull(0, AST::FundamentalTypeModifierList))) {
                        switch (m->getSubType<ast::FundamentalTypeModifier>()) {
                            case ast::FundamentalTypeModifier::Signed:
                                if (isSigned || isUnsigned) return invalidModifier(m);
                                isSigned = true;
                                break;
                            case ast::FundamentalTypeModifier::Unsigned:
                                if (isSigned || isUnsigned) return invalidModifier(m);
                                isUnsigned = true;
                                break;
                            case ast::FundamentalTypeModifier::Long:
                                if (isShort || (isLong > 1)) return invalidModifier(m);
                                ++isLong;
                                break;
                            case ast::FundamentalTypeModifier::Short:
                                if (isShort || isLong) return invalidModifier(m);
                                isShort = true;
                                break;
                        }
                    }
                    if (isUnsigned) {
                        if (isShort) return Type::getUnsignedShort(*program);
                        if (isLong > 1) return Type::getUnsignedLongLong(*program);
                        if (isLong) return Type::getUnsignedLong(*program);
                        return Type::getUnsignedInt(*program);
                    } else {
                        if (isShort) return Type::getShort(*program);
                        if (isLong > 1) return Type::getLongLong(*program);
                        if (isLong) return Type::getLong(*program);
                        return Type::getInt(*program);
                    }
                }
            }
            return nullptr;
        }
        default: addError(ast, "invalid AST"); return nullptr;
    }
}
//---------------------------------------------------------------------------
const Type* SemanticAnalysis::analyzeTypeIdExpression(Scope& scope, const AST* ast)
// Analyze an id-expression with optional pointer markers
{
    if (ast->getType() == AST::TypeModifier) {
        switch (ast->getSubType<ast::TypeModifier>()) {
            case ast::TypeModifier::Pointer: {
                auto t = analyzeTypeIdExpression(scope, ast->getAny(0));
                return t ? t->getPointerTo() : nullptr;
            }
            case ast::TypeModifier::Const:
                return analyzeIdExpression(scope, ast);
        }
        return nullptr;
    } else {
        return analyzeIdExpression(scope, ast);
    }
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeUnnamedDeclaration(Scope& scope, const AST* ast, const Type** type, unique_ptr<Expression>* value)
// Analyze an unnamed declaration
{
    auto statement = ast->getAnyOrNull(1);
    if (ast->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function) {
        // TODO
        addError(ast, "nested function declarations not supported yet");
        return false;
    } else {
        *type = nullptr;
        if (ast->getAny(0)) {
            if (!((*type = analyzeTypeIdExpression(scope, ast->getAny(0)))))
                return false;
        } else if (!statement) {
            addError(ast, "a deduced type must have an = initializer");
            return false;
        }
        value->reset();
        if (statement) {
            if (statement->getType() != AST::ExpressionStatement) {
                addError(statement, "invalid initializer");
                return false;
            }
            if (!((*value = analyzeExpression(scope, statement->getAny(0)))))
                return false;
        }
    }
    return true;
}
//---------------------------------------------------------------------------
const FunctionType* SemanticAnalysis::analyzeFunctionType(Scope& scope, const AST* ast, vector<unique_ptr<Expression>>* defaultArguments, unsigned* defaultArgumentsOffset)
// Analyze a function type declaration
{
    auto parameterList = ast->get(0, AST::ParameterDeclarationList);
    bool throwsSpecifier = ast->getAnyOrNull(1);
    auto returnList = ast->getOrNull(2, AST::ReturnList);
    auto contractList = ast->getOrNull(3, AST::ContractSeq);

    // Inspect all parameters
    vector<FunctionType::Parameter> parameter;
    if (defaultArguments) {
        defaultArguments->clear();
        *defaultArgumentsOffset = 0;
    }
    unsigned slot = 0;
    for (auto p : List<AST::ParameterDeclaration>(parameterList->getOrNull(0, AST::ParameterDeclarationSeq))) {
        FunctionType::Parameter pa;
        if (auto d = p->getOrNull(0, AST::ParameterDirection)) {
            switch (d->getSubType<ast::ParameterDirection>()) {
                case ast::ParameterDirection::In: pa.direction = FunctionType::ParameterDirection::In; break;
                case ast::ParameterDirection::Out: pa.direction = FunctionType::ParameterDirection::Out; break;
                case ast::ParameterDirection::Inout: pa.direction = FunctionType::ParameterDirection::Inout; break;
                case ast::ParameterDirection::Copy: pa.direction = FunctionType::ParameterDirection::Copy; break;
                case ast::ParameterDirection::Move: pa.direction = FunctionType::ParameterDirection::Move; break;
                case ast::ParameterDirection::Forward: pa.direction = FunctionType::ParameterDirection::Forward; break;
            }
        }
        auto d = p->getOrNull(1, AST::Declaration);
        unique_ptr<Expression> defaultArgument;
        if (d) {
            pa.name = extractIdentifier(d->getAny(0));
            if (!analyzeUnnamedDeclaration(scope, d->get(1, AST::UnnamedDeclaration), &pa.type, &defaultArgument)) return nullptr;
        } else {
            if (slot) {
                addError(p, "this must be the first argument");
                return nullptr;
            }
            auto cl = dynamic_cast<Class*>(scope.getCurrentNamespace());
            if (!cl) {
                addError(p, "member functions can only be declared inside classes");
                return nullptr;
            }
            pa.name = "this";
            pa.type = cl->getType();
        }
        if (defaultArguments) {
            if (defaultArgument) {
                if (defaultArguments->empty()) *defaultArgumentsOffset = slot;
                defaultArguments->push_back(move(defaultArgument));
            } else if (!defaultArguments->empty()) {
                addError(p, "default argument missing");
                return nullptr;
            }
        }
        parameter.push_back(move(pa));
        ++slot;
    }

    // Handle the return type
    vector<pair<string, const Type*>> returnTypes;
    if (returnList) {
        if (returnList->getSubType<ast::ReturnList>() == ast::ReturnList::Single) {
            auto rt = analyzeIdExpression(scope, returnList->getAny(0));
            if (!rt) return nullptr;
            returnTypes.emplace_back(""sv, rt);
        } else {
            auto pdl = returnList->get(0, AST::ParameterDeclarationList);
            for (auto r : List<AST::ParameterDeclaration>(pdl->getOrNull(0, AST::ParameterDeclarationSeq))) {
                if (r->getAny(0)) {
                    addError(r->getAny(0), "direction modifier not allowed in return list");
                    return nullptr;
                }
                auto d = ast->get(1, AST::Declaration);
                auto name = extractIdentifier(d->getAny(0));
                const Type* type;
                unique_ptr<Expression> defaultArgument;
                if (!analyzeUnnamedDeclaration(scope, d->get(1, AST::UnnamedDeclaration), &type, &defaultArgument)) return nullptr;
                if (defaultArgument) {
                    addError(d, "return value cannot have default values");
                    return nullptr;
                }
                returnTypes.emplace_back(name, type);
            }
        }
    }

    // Contracts are not supported yet
    if (contractList) {
        // TODO
        addError(contractList, "contracts not implemented yet");
        return nullptr;
    }

    return FunctionType::get(*program, move(parameter), move(returnTypes), throwsSpecifier);
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeDeclaration(Scope& scope, const AST* declaration)
// Analyze a declaration
{
    auto loc = mapping.getBegin(declaration->getRange());
    auto name = extractDeclarationId(declaration->getAny(0));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    // Check if the declaration already exists
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (decl) {
        // That is only permissive for functions
        if ((!isFunction) || (!decl->isFunction()))
            return addError(declaration, "duplicate definition");
    } else {
        if (isFunction) {
            decl = scope.getCurrentNamespace()->addDeclaration(make_unique<FunctionDeclaration>(loc, move(name)));
        } else {
            if (!details->getAnyOrNull(0)) return addError(declaration, "type required");
            auto type = analyzeTypeIdExpression(scope, details->getAnyOrNull(0));
            if (!type) return false;
            decl = scope.getCurrentNamespace()->addDeclaration(make_unique<VariableDeclaration>(loc, move(name), type));
        }
    }

    // Analyze the signature
    unsigned slot = 0;
    if (isFunction) {
        auto fdecl = static_cast<FunctionDeclaration*>(decl);
        vector<unique_ptr<Expression>> defaultArguments;
        unsigned defaultArgumentsOffset;
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), &defaultArguments, &defaultArgumentsOffset);
        if (!funcType) return false;
        auto existing = fdecl->findFunctionOverload(funcType);
        if (existing) {
            if (existing->type == funcType)
                return addError(declaration, "function overload with that signature already exists");
            return addError(declaration, "function overload with ambiguous signature already exists");
        }
        slot = fdecl->addFunctionOverload(loc, funcType, move(defaultArguments), defaultArgumentsOffset);
    }
    if (!inStdlib) program->trackSourceOrder(decl, slot);

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeDefinition(Scope& scope, const AST* declaration)
// Analyze a definition
{
    auto name = extractDeclarationId(declaration->getAny(0));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (isFunction) {
        if (inStdlib && (!details->getAnyOrNull(1))) return true;

        auto fdecl = static_cast<FunctionDeclaration*>(decl);
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), nullptr, nullptr);
        auto overload = fdecl->findFunctionOverload(funcType);

        FunctionScope fs(overload->type);
        Scope innerScope(scope);
        innerScope.setCurrentFunction(&fs);
        overload->statement = analyzeStatement(innerScope, details->getAny(1));
        return !!(overload->statement);
    } else {
        // Declaration only?
        if (!details->getAnyOrNull(1)) {
            if (inStdlib) return true;
            return addError(declaration, "a global declaration must be initialized");
        }

        // TODO
        addError(declaration, "variable declarations not implemented yet");
    }

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeNamespace(Scope& scope, const AST* ast, Phase phase)
// Analyze a namespace
{
    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<NamespaceDeclaration>(mapping.getBegin(ast->getRange()), name, scope.getCurrentNamespace()));
    } else if (decl->getCategory() != Declaration::Category::Namespace) {
        return addError(ast, "duplicate definition");
    }
    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);

    {
        Scope innerScope(scope);
        innerScope.setCurrentNamespace(static_cast<NamespaceDeclaration*>(decl)->getNamespace());
        if (!analyzeDeclarations(innerScope, ast->getAnyOrNull(1), phase)) return false;
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 1);

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeClass(Scope& scope, const AST* ast, Phase phase)
// Analyze a class definition
{
    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<ClassDeclaration>(mapping.getBegin(ast->getRange()), name, scope.getCurrentNamespace(), program.get()));
    } else if (phase == Phase::Declarations) {
        return addError(ast, "duplicate definition");
    }
    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);

    if (ast->getAnyOrNull(1)) {
        // TODO
        return addError(ast, "inheritance not implemented yet");
    }

    {
        Scope innerScope(scope);
        innerScope.setCurrentNamespace(static_cast<ClassDeclaration*>(decl)->getClass());
        if (!analyzeDeclarations(innerScope, ast->getAnyOrNull(2), phase)) return false;
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 1);

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeUsing(Scope& scope, const AST* ast, Phase phase, bool usingDecltype)
// Analyze a typedef
{
    if (phase != Phase::Declarations) return true;

    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    const Type* type;
    if (!usingDecltype) {
        if (!(type = analyzeTypeIdExpression(scope, ast->getAny(1)))) return false;
    } else {
        Scope innerScope(scope); // TODO set to ignore lifetime checks
        auto e = analyzeExpression(innerScope, ast->getAny(1));
        if (!e) return false;
        type = e->getType();
    }
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<TypedefDeclaration>(mapping.getBegin(ast->getRange()), name, type));
    } else {
        return addError(ast, "duplicate definition");
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeDeclarations(Scope& scope, const AST* declarations, Phase phase)
// Analyze a parse tree
{
    // Process all declarations
    for (auto d : ASTList(declarations))
        switch (d->getType()) {
            case AST::Type::Declaration:
                if (!((phase == Phase::Declarations) ? analyzeDeclaration(scope, d) : analyzeDefinition(scope, d))) return false;
                break;
            case AST::Type::Namespace:
                if (!analyzeNamespace(scope, d, phase)) return false;
                break;
            case AST::Type::Class:
                if (!analyzeClass(scope, d, phase)) return false;
                break;
            case AST::Type::Using:
            case AST::Type::UsingDecltype:
                if (!analyzeUsing(scope, d, phase, d->getType() == AST::Type::UsingDecltype)) return false;
                break;
            case AST::Type::Template: return addError(d, "template not implemented yet");
            default: return addError(d, "invalid AST");
        }

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyze(const AST* translationUnit)
// Analyze a parse tree
{
    assert(translationUnit && translationUnit->getType() == AST::TranslationUnit);
    auto declarations = translationUnit->getOrNull(0, AST::DeclarationSeq);

    ScopeRoot scopeRoot;
    Scope scope(scopeRoot);
    scope.setCurrentNamespace(target->getGlobalNamespace());

    // Process all declarations
    if (!analyzeDeclarations(scope, declarations, Phase::Declarations)) return false;

    // Now process all definitions
    if (!analyzeDeclarations(scope, declarations, Phase::Definitions)) return false;

    return true;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
