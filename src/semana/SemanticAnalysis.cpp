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
#include <unordered_set>
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
void SemanticAnalysis::throwError(SourceLocation loc, string text)
// Store an error message
{
    errors.emplace_back(loc, move(text));
    throw Error();
}
//---------------------------------------------------------------------------
void SemanticAnalysis::throwError(const AST* loc, std::string text)
// Store an error message
{
    throwError(mapping.getBegin(loc->getRange()), move(text));
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
            case ast::OperatorName::Assignment: return DeclarationId(DeclarationId::Category::OperatorAssignment);
            case ast::OperatorName::BitAnd: return DeclarationId(DeclarationId::Category::OperatorBitAnd);
            case ast::OperatorName::BitAndEq: return DeclarationId(DeclarationId::Category::OperatorBitAndEq);
            case ast::OperatorName::BitOr: return DeclarationId(DeclarationId::Category::OperatorBitOr);
            case ast::OperatorName::BitOrEq: return DeclarationId(DeclarationId::Category::OperatorBitOrEq);
            case ast::OperatorName::BitXor: return DeclarationId(DeclarationId::Category::OperatorBitXor);
            case ast::OperatorName::BitXorEq: return DeclarationId(DeclarationId::Category::OperatorBitXorEq);
            case ast::OperatorName::Brackets: return DeclarationId(DeclarationId::Category::OperatorBrackets);
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
void SemanticAnalysis::enforceConvertible(const AST* loc, std::unique_ptr<Expression>& exp, const Type* target, [[maybe_unused]] bool explicitScope)
// Make sure an expression is convertible into a certain type
{
    auto ta = exp->getType()->getEffectiveType();
    auto tb = target->getEffectiveType();

    // Trivial conversion?
    if (ta == tb) return;

    // Standard conversions
    if (tb->isPointerType()) {
        if (ta->isFundamentalType() && (tb->as<FundamentalType>()->getId() == Type::FundamentalTypeId::NullptrType))
            return;
        // TODO handle const
    } else if (tb->isFundamentalType()) {
        auto ib = tb->as<FundamentalType>()->getId();
        if (ib == Type::FundamentalTypeId::Bool) {
            if (ta->isPointerType()) return;
        }
        if (ta->isFundamentalType()) {
            auto ia = ta->as<FundamentalType>()->getId();
            // So far all conversions are valid as long as both are not void
            if ((ia != Type::FundamentalTypeId::Void) && (ib != Type::FundamentalTypeId::Void) && (ia != Type::FundamentalTypeId::NullptrType) && (ib != Type::FundamentalTypeId::NullptrType))
                return;
        }
    }

    // TODO Check user defined conversions
    throwError(loc, "no type conversion found");
}
//---------------------------------------------------------------------------
pair<const Type*, ValueCategory> SemanticAnalysis::resolveOperator([[maybe_unused]] Scope& scope, const AST* ast, const DeclarationId& id, const Expression& left, const Expression& right)
// Try to resolve an operator
{
    // Prefer methods if any
    auto leftType = left.getType();
    if (leftType->isClassType()) {
        auto ct = leftType->as<ClassType>();
        auto cl = ct->getClass();
        if (auto dec = cl->findWithInheritance(id)) {
            array<CallArg, 2> args({{left.getType(), left.getValueCategory(), CallArg::Regular}, {right.getType(), right.getValueCategory(), CallArg::Regular}});
            array<FunctionDeclaration*, 1> candidates({dec});
            auto e = resolveCall(ast, candidates, args, false);
            if (e.has_value()) {
                auto ft = e->first->accessOverload(e->second).type;
                if (ft->getReturnValues().empty()) {
                    return {Type::getVoid(*program), ValueCategory::Prvalue};
                } else if (ft->getReturnValues().size() != 1) {
                    throwError(ast, "operator must return a single value");
                } else {
                    auto t = ft->getReturnValues()[0].second;
                    return {t, (ft->hasFlag(FunctionType::ReturnsRef) ? ValueCategory::Lvalue : ValueCategory::Prvalue)};
                }
            }
        }
    }

    // Regular lookup
    return {nullptr, ValueCategory::Prvalue}; // TODO
}
//---------------------------------------------------------------------------
Declaration* SemanticAnalysis::resolveUnqualifiedId(Scope& scope, const AST* ast)
// Resolve an unqualified id
{
    auto subType = ast->getSubType<ast::UnqualifiedId>();
    if (subType == ast::UnqualifiedId::Template)
        throwError(ast, "templates not implemented yet"); // TODO
    auto name = DeclarationId(extractIdentifier(ast->get(0, AST::Identifier)));
    auto ns = scope.getCurrentNamespace();
    while (ns) {
        // Within a class, the unqualified name refers to the class and not the constructor
        if (dynamic_cast<Class*>(ns) && ns->getName() == name) ns = ns->getParent();

        auto d = ns->findDeclaration(name);
        if (d) return d;
        ns = ns->getParent();
    }
    throwError(ast, "unknown type name '" + name.name + "'");
}
//---------------------------------------------------------------------------
Declaration* SemanticAnalysis::resolveQualifiedId(Scope& scope, const AST* ast)
// Resolve a qualified id
{
    if (ast->getSubType<ast::QualifiedId>() != ast::QualifiedId::Nested)
        throwError(ast, "type name expected");

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
        if (subType == ast::UnqualifiedId::Template)
            throwError(e, "templates not implemented yet");
        auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

        auto d = ns->resolveDeclaration(name);
        if (!d)
            throwError(e, "'" + name.name + "' not found in namespace '" + ns->getName() + "'");
        Namespace* next;
        if (auto n = dynamic_cast<NamespaceDeclaration*>(d)) {
            next = n->getNamespace();
        } else if (auto c = dynamic_cast<ClassDeclaration*>(d)) {
            next = c->getClass();
        }
        if (!next)
            throwError(e, "'" + name.name + "' is not a namespace");
        ns = next;
    }

    // Lookup the element itself
    auto e = ast->get(1, AST::UnqualifiedId);
    auto subType = e->getSubType<ast::UnqualifiedId>();
    if (subType == ast::UnqualifiedId::Template)
        throwError(ast, "templates not implemented yet"); // TODO
    auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

    auto d = ns->resolveDeclaration(name);
    if (!d)
        throwError(e, "'" + name.name + "' not found in namespace '" + ns->getName() + "'");
    return d;
}
//---------------------------------------------------------------------------
std::unique_ptr<Literal> SemanticAnalysis::deriveConstexpr(const Expression& exp)
// Derive a constexpr value
{
    if (exp.getCategory() != Expression::Category::Literal)
        throwError(exp.getLocation(), "constexpr not implemented yet");
    auto& l = static_cast<const Literal&>(exp);
    return make_unique<Literal>(l);
}
//---------------------------------------------------------------------------
optional<pair<FunctionDeclaration*, unsigned>> SemanticAnalysis::resolveCall(const AST* ast, std::span<FunctionDeclaration*> candidates, std::span<const CallArg> args, bool reportErrors)
// Find the function to call
{
    enum class Matching { Exact,
                          Promotion,
                          Conversion };
    auto isNumerical = [](const Type* t) {
        return t->isFundamentalType() && Type::isNumerical(static_cast<const FundamentalType*>(t)->getId());
    };

    FunctionDeclaration* bestCandidate = nullptr;
    unsigned bestOverloadSlot = 0;
    bool bestIsAmbiguous = false;
    vector<Matching> bestMatching, currentMatching;
    bestMatching.resize(args.size());
    currentMatching.resize(args.size());
    for (auto d : candidates) {
        for (unsigned slot = 0, limit = d->getOverloadCount(); slot < limit; ++slot) {
            auto& o = d->accessOverload(slot);
            auto& p = o.type->getParameters();
            if ((args.size() <= p.size()) && (args.size() + o.defaultArguments.size() >= p.size())) {
                // Could be a candidate, check all arguments
                bool match = true, dominate = false;
                for (unsigned argSlot = 0; argSlot < p.size(); ++argSlot) {
                    Matching matching;
                    if (p[argSlot].direction == FunctionType::ParameterDirection::Out) {
                        // For out the type must match precisely and we need an lvalue
                        if ((!p[argSlot].type->isEquivalentTo(args[argSlot].type)) || (args[argSlot].category != ValueCategory::Lvalue)) {
                            match = false;
                            break;
                        }
                        matching = Matching::Exact;
                    } else if (p[argSlot].direction == FunctionType::ParameterDirection::Inout) {
                        // For inout it most be a non-const identical or derived type and we need an lvalue except for this. TODO handle derived types
                        if ((!p[argSlot].type->isEquivalentTo(args[argSlot].type)) || ((args[argSlot].category != ValueCategory::Lvalue) && (!((argSlot == 0) && (p[argSlot].name == "this"sv))))) {
                            match = false;
                            break;
                        }
                        matching = Matching::Exact;
                    } else {
                        // Otherwise we have to consider type promotion
                        if (p[argSlot].type->isEquivalentTo(args[argSlot].type)) {
                            matching = Matching::Exact;
                        } else if (isNumerical(p[argSlot].type) && isNumerical(args[argSlot].type)) {
                            matching = Matching::Promotion;
                        } else {
                            // TODO consider type conversions
                            match = false;
                            break;
                        }
                    }

                    // Compare with the best candidate so far
                    if (bestCandidate && (bestMatching[argSlot] != matching)) {
                        if (bestMatching[argSlot] < matching) {
                            match = false;
                            break;
                        } else
                            dominate = true;
                    }
                    currentMatching[argSlot] = matching;
                }

                if (match) {
                    // We found a match
                    if ((!bestCandidate) || dominate) {
                        bestCandidate = d;
                        bestOverloadSlot = slot;
                        bestMatching = currentMatching;
                        bestIsAmbiguous = false;
                    } else {
                        bestIsAmbiguous = true;
                    }
                }
            }
        }
    }

    // Did we find a candidate
    if (bestCandidate) {
        // Ambiguous?
        if (bestIsAmbiguous) {
            if (reportErrors) throwError(ast, "call is ambiguous");
            return nullopt;
        }

        // Check if the modifiers match
        auto& o = bestCandidate->accessOverload(bestOverloadSlot);
        for (unsigned index = 0, limit = args.size(); index < limit; ++index) {
            switch (o.type->getParameters()[index].direction) {
                case FunctionType::ParameterDirection::In:
                case FunctionType::ParameterDirection::Inout:
                    if (args[index].modifier == CallArg::Modifier::Out) {
                        if (reportErrors) throwError(ast, "modifier 'out' not allowed in argument " + to_string(index + 1));
                        return nullopt;
                    }
                    if (args[index].modifier == CallArg::Modifier::Move) {
                        if (reportErrors) throwError(ast, "modifier 'move' not allowed in argument " + to_string(index + 1));
                        return nullopt;
                    }
                    break;
                case FunctionType::ParameterDirection::Out:
                    if (args[index].modifier != CallArg::Modifier::Out) {
                        if (reportErrors) throwError(ast, "modifier 'out' required in argument " + to_string(index + 1));
                        return nullopt;
                    }
                    break;
                case FunctionType::ParameterDirection::Move:
                    if (args[index].modifier != CallArg::Modifier::Move) {
                        if (reportErrors) throwError(ast, "modifier 'move' required in argument " + to_string(index + 1));
                        return nullopt;
                    }
                    break;
                case FunctionType::ParameterDirection::Copy:
                case FunctionType::ParameterDirection::Forward:
                    if (args[index].modifier == CallArg::Modifier::Out) {
                        if (reportErrors) throwError(ast, "modifier 'out' not allowed in argument " + to_string(index + 1));
                        return nullopt;
                    }
                    break;
            }
        }
        return make_pair(bestCandidate, bestOverloadSlot);
    }

    return nullopt;
}
//---------------------------------------------------------------------------
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpression(Scope& scope, const AST* ast, const Type* typeHint)
// Analyze an expression
{
    switch (ast->getType()) {
        case AST::ExpressionListExpression: return analyzeExpressionListExpression(scope, ast, typeHint);
        case AST::AssignmentExpression: return analyzeAssignmentExpression(scope, ast);
        case AST::BinaryExpression: return analyzeBinaryExpression(scope, ast);
        case AST::PrefixExpression: throwError(ast, "prefix_expression not implemented yet"); // TODO
        case AST::PostfixExpression: throwError(ast, "postfix_expression not implemented yet"); // TODO
        case AST::BracketExpression: throwError(ast, "bracket_expression not implemented yet"); // TODO
        case AST::ParenExpression: throwError(ast, "paren_expression not implemented yet"); // TODO
        case AST::DotExpression: throwError(ast, "dot_expression not implemented yet"); // TODO
        case AST::InspectExpression: throwError(ast, "inspect_expression not implemented yet"); // TODO
        case AST::Literal: return analyzeLiteral(ast);
        case AST::UnqualifiedId:
        case AST::QualifiedId: return analyzeIdExpressionExpression(scope, ast);
        case AST::UnnamedDeclaration: throwError(ast, "lambda expressions not implemented yet"); // TODO
        case AST::NewExpression: throwError(ast, "new expressions not implemented yet"); // TODO
        default: throwError(ast, "invalid AST");
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
        if (v > limit)
            throwError(ast, "integer value out of range");
        return make_unique<Literal>(loc, type, text);
    };

    // Interpret the literal
    switch (ast->getSubType<ast::Literal>()) {
        case ast::Literal::True: return make_unique<Literal>(loc, Type::getBool(*program), text);
        case ast::Literal::False: return make_unique<Literal>(loc, Type::getBool(*program), text);
        case ast::Literal::Nullptr: return make_unique<Literal>(loc, Type::getNullptrType(*program), text);
        case ast::Literal::CharLiteral: return make_unique<Literal>(loc, Type::getChar(*program), text);
        case ast::Literal::StringLiteral: return make_unique<Literal>(loc, Type::getChar(*program)->getAsConst()->getPointerTo(), text);
        case ast::Literal::DecimalInteger: {
            uint64_t v = 0;
            unsigned index = 0;
            for (; index < text.length(); ++index) {
                char c = text[index];
                if (c == '\'') continue;
                if ((c < '0') || (c > '9')) break;
                uint64_t v1 = 10 * v, v2 = v1 + (c - '0');
                if ((v1 < v) || (v2 < v1))
                    throwError(ast, "integer value out of range");
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
                if ((v1 < v) || (v2 < v1))
                    throwError(ast, "integer value out of range");
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
                if ((v1 < v) || (v2 < v1))
                    throwError(ast, "integer value out of range");
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
    throwError(ast, "invalid AST");
}
//---------------------------------------------------------------------------
static bool canConvertImplicit(const Type* targetType, const Type* sourceType)
// Check for implicit conversions
{
    if (targetType == sourceType) return true;
    using FundamentalTypeId = Type::FundamentalTypeId;
    if (targetType->isFundamentalType() && sourceType->isFundamentalType()) {
        auto targetId = targetType->as<FundamentalType>()->getId();
        auto sourceId = sourceType->as<FundamentalType>()->getId();

        if (Type::isNumerical(targetId) && Type::isNumerical(sourceId)) {
            // We can always convert numbers to the target type
            return true;
        }
    } else if (targetType->isPointerType()) {
        if (sourceType->isFundamentalType() && sourceType->as<FundamentalType>()->getId() == FundamentalTypeId::NullptrType) {
            // Nullptr can be converted into anything
        } else if (sourceType->isPointerType()) {
            auto te = targetType->as<PointerType>()->getElementType();
            auto se = sourceType->as<PointerType>()->getElementType();
            if (se->isConst() && !te->isConst()) return false;
            te = te->getAsNonConst();
            se = se->getAsNonConst();
            return se == te;
        }
    }
    return false;
}
//---------------------------------------------------------------------------
unique_ptr<Expression> SemanticAnalysis::analyzeAssignmentExpression(Scope& scope, const AST* ast)
// Analyze a binary expression
{
    // Interpret the AST node type
    auto loc = mapping.getBegin(ast->getRange());
    auto subType = ast->get(1, AST::AssignmentOperator)->getSubType<ast::AssignmentOperator>();
    AssignmentExpression::Op op;
    DeclarationId::Category id;
    switch (subType) {
        case ast::AssignmentOperator::Assignment:
            op = AssignmentExpression::Assignment;
            id = DeclarationId::OperatorAssignment;
            break;
        case ast::AssignmentOperator::MultiplyEq:
            op = AssignmentExpression::MulEq;
            id = DeclarationId::OperatorMulEq;
            break;
        case ast::AssignmentOperator::SlashEq:
            op = AssignmentExpression::DivEq;
            id = DeclarationId::OperatorDivEq;
            break;
        case ast::AssignmentOperator::ModuloEq:
            op = AssignmentExpression::ModuloEq;
            id = DeclarationId::OperatorModuloEq;
            break;
        case ast::AssignmentOperator::PlusEq:
            op = AssignmentExpression::PlusEq;
            id = DeclarationId::OperatorPlusEq;
            break;
        case ast::AssignmentOperator::MinusEq:
            op = AssignmentExpression::MinusEq;
            id = DeclarationId::OperatorPlusEq;
            break;
        case ast::AssignmentOperator::RightShiftEq:
            op = AssignmentExpression::RightShiftEq;
            id = DeclarationId::OperatorRightShiftEq;
            break;
        case ast::AssignmentOperator::LeftShiftEq:
            op = AssignmentExpression::LeftShiftEq;
            id = DeclarationId::OperatorLeftShiftEq;
            break;
        case ast::AssignmentOperator::AmpersandEq:
            op = AssignmentExpression::BitAndEq;
            id = DeclarationId::OperatorBitAndEq;
            break;
        case ast::AssignmentOperator::CaretEq:
            op = AssignmentExpression::BitXorEq;
            id = DeclarationId::OperatorBitXorEq;
            break;
        case ast::AssignmentOperator::PipeEq:
            op = AssignmentExpression::BitOrEq;
            id = DeclarationId::OperatorBitOrEq;
            break;
    }

    // Assignment is special, as we allow for uninitializing objects here
    unique_ptr<Expression> left;
    Scope::Var* varToInitialize = nullptr;
    if (op == AssignmentExpression::Assignment) {
        auto checkForUnitializedLocalVar = [this](Scope& scope, const AST* ast) -> Scope::Var* {
            if (ast->getType() != AST::UnqualifiedId) return nullptr;
            if (ast->getSubType<ast::UnqualifiedId>() == ast::UnqualifiedId::Template) return nullptr;
            string name(extractIdentifier(ast->get(0, AST::Identifier)));
            auto var = scope.resolveVariable(name);
            return (var && !var->initialized) ? var : nullptr;
        };

        if (auto arg = checkForUnitializedLocalVar(scope, ast->getAny(0))) {
            varToInitialize = arg;
            op = AssignmentExpression::Construct;
            left = make_unique<VariableExpression>(mapping.getBegin(ast->getAny(0)->getRange()), arg->type, string(extractIdentifier(ast->getAny(0)->get(0, AST::Identifier))), nullptr);
        } else {
            left = analyzeExpression(scope, ast->getAny(0));
        }
    } else {
        left = analyzeExpression(scope, ast->getAny(0));
    }
    if (!left) return {};

    // Check the input
    auto right = analyzeExpression(scope, ast->getAny(2));
    if (!right) return {};

    // The variable is initialized afterwards
    if (varToInitialize) {
        if (!varToInitialize->initialized) {
            scope.markInitialized(ast, varToInitialize);
        } else {
            // Our right side might contain another initialization...
            op = AssignmentExpression::Assignment;
            left = make_unique<WrappedVariableExpression>(mapping.getBegin(ast->getAny(0)->getRange()), left->getType(), string(extractIdentifier(ast->getAny(0)->get(0, AST::Identifier))), nullptr);
        }
    }

    // Check for overloaded operators
    DeclarationId fid(id);
    auto match = resolveOperator(scope, ast, fid, *left, *right);
    if (match.first) return make_unique<AssignmentExpression>(loc, match.first, match.second, op, move(left), move(right));

    // Check for builtin assignment functionality
    const Type* leftType = left->getType();
    const Type* rightType = right->getType();
    if ((!leftType->isConst()) && (!leftType->isClassType()) && canConvertImplicit(leftType, rightType) && ((op == AssignmentExpression::Assignment) || (leftType->isFundamentalType())))
        return make_unique<AssignmentExpression>(loc, leftType, Expression::ValueCategory::Lvalue, op, move(left), move(right));

    if (leftType->isConst()) {
        throwError(ast, "assignment of read-only variable");
    } else {
        throwError(ast, "assignment operator not supported for data types");
    }
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
        case ast::BinaryExpression::Is: throwError(ast, "is not implemented yet"); // TODO
        case ast::BinaryExpression::As: throwError(ast, "as not implemented yet"); // TODO
    }

    // Check the input
    auto left = analyzeExpression(scope, ast->getAny(0));
    if (!left) return {};
    auto right = analyzeExpression(scope, ast->getAny(1));
    if (!right) return {};

    // Check for overloaded operators
    DeclarationId fid(id);
    auto match = resolveOperator(scope, ast, fid, *left, *right);
    if (match.first) return make_unique<BinaryExpression>(loc, match.first, match.second, op, move(left), move(right));

    // In case of comparisons, try again with spaceship
    if (isCmp) {
        match = resolveOperator(scope, ast, DeclarationId::OperatorSpaceship, *left, *right);
        if (match.first) return make_unique<BinaryExpression>(loc, Type::getBool(*program), Expression::ValueCategory::Prvalue, op, move(left), move(right));
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
            enforceConvertible(ast, left, Type::getBool(*program));
            enforceConvertible(ast, right, Type::getBool(*program));
            return make_unique<BinaryExpression>(loc, Type::getBool(*program), Expression::ValueCategory::Prvalue, op, move(left), move(right));
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
            if ((leftType == rightType) && Type::isInteger(leftId)) return make_unique<BinaryExpression>(loc, leftType, Expression::ValueCategory::Prvalue, op, move(left), move(right));
            break;
        case BinaryExpression::Equal:
        case BinaryExpression::NotEqual:
        case BinaryExpression::Less:
        case BinaryExpression::LessEq:
        case BinaryExpression::Greater:
        case BinaryExpression::GreaterEq:
        case BinaryExpression::Spaceship:
            if (((leftType == rightType) && leftType->isPointerType()) || ((Type::isNumerical(leftId) && Type::isNumerical(rightId)))) return make_unique<BinaryExpression>(loc, Type::getBool(*program), Expression::ValueCategory::Prvalue, op, move(left), move(right));
            break;
        case BinaryExpression::Plus:
        case BinaryExpression::Minus:
            if (leftType->isPointerType() || rightType->isPointerType())
                throwError(ast, "pointer arithmetic is illegal - use std::span");
            [[fallthrough]];
        case BinaryExpression::Mul:
        case BinaryExpression::Div:
        case BinaryExpression::Modulo:
            if ((leftType == rightType) && (Type::isNumerical(leftId))) return make_unique<BinaryExpression>(loc, leftType, Expression::ValueCategory::Prvalue, op, move(left), move(right));
            break;
    }

    throwError(ast, "binary operator not supported for data types");
}
//---------------------------------------------------------------------------
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpressionListExpression(Scope& scope, const AST* ast, [[maybe_unused]] const Type* typeHint)
// Analyze an expression
{
    // Check if this is something else than a simple value
    if ((!ast->getAnyOrNull(0)) || (ast->getAnyOrNull(0)->getAnyOrNull(1)))
        throwError(ast, "implicit object construction not implemented yet"); // TODO

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
        if (subType == ast::UnqualifiedId::Template)
            throwError(ast, "templates not implemented yet"); // TODO
        localId = DeclarationId(extractIdentifier(ast->get(0, AST::Identifier)));

        // A local variable?
        if (auto localVar = scope.resolveVariable(localId.name)) {
            if (!localVar->initialized)
                throwError(ast, string(localId.name) + " is used uninitialized here");
            if (localVar->wrapped)
                return make_unique<WrappedVariableExpression>(loc, localVar->type, localId.name, nullptr);
            return make_unique<VariableExpression>(loc, localVar->type, localId.name, nullptr);
        }
    } else {
        if (ast->getSubType<ast::QualifiedId>() != ast::QualifiedId::Nested)
            throwError(ast, "type name expected");

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
            if (subType == ast::UnqualifiedId::Template)
                throwError(e, "templates not implemented yet"); // TODO
            auto name = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));

            auto d = ns->findDeclaration(name);
            if (!d)
                throwError(e, "'" + name.name + "' not found in namespace '" + ns->getName() + "'");
            Namespace* next;
            if (auto n = dynamic_cast<NamespaceDeclaration*>(d)) {
                next = n->getNamespace();
            } else if (auto c = dynamic_cast<ClassDeclaration*>(d)) {
                next = c->getClass();
            }
            if (!next)
                throwError(e, "'" + name.name + "' is not a namespace");
            ns = next;
        }

        // The last part of the path
        auto e = ast->get(1, AST::UnqualifiedId);
        auto subType = e->getSubType<ast::UnqualifiedId>();
        if (subType == ast::UnqualifiedId::Template)
            throwError(ast, "templates not implemented yet"); // TODO
        localId = DeclarationId(extractIdentifier(e->get(0, AST::Identifier)));
    }

    // Lookup the element itself
    auto d = ns->findDeclaration(localId);
    if (!d)
        throwError(ast, "'" + localId.name + "' not found in namespace '" + ns->getName() + "'");
    if (d->getCategory() != Declaration::Category::Variable)
        throwError(ast, "'" + localId.name + "' is not a variable");
    auto v = static_cast<VariableDeclaration*>(d);
    return make_unique<VariableExpression>(loc, v->getType(), v->getName().name, v->getContainingNamespace());
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeDeclarationStatement(Scope& scope, const AST* ast)
// Analyze a declaration statement
{
    auto begin = mapping.getBegin(ast->getRange());
    auto decl = ast->get(0, AST::Declaration);
    switch (decl->getType()) {
        case AST::Declaration: {
            // Check the name
            auto name = extractDeclarationId(decl->getAny(0));
            if (!name.isRegular())
                throwError(decl, "operator definition not allowed here");
            auto details = decl->get(1, AST::UnnamedDeclaration);
            bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;
            if (isFunction)
                throwError(decl, "local function definitions not implemented yet"); // TODO
            if (scope.definesVariable(name.name))
                throwError(decl, "duplicate definition of '" + name.name + "'");

            // Check the type (if any)
            const Type* type = nullptr;
            if (details->getAnyOrNull(0)) {
                type = analyzeTypeIdExpression(scope, details->getAnyOrNull(0));
            }

            // Check the initial value (if any)
            unique_ptr<Expression> init;
            if (details->getAnyOrNull(1)) {
                if (details->getAny(1)->getType() != AST::ExpressionStatement)
                    throwError(details->getAny(1), "expression required");
                init = analyzeExpression(scope, details->getAny(1)->getAny(0), type);
                if (!init) return {};
                if (!type) {
                    type = init->getType();
                } else if (!canConvertImplicit(type, init->getType())) {
                    VariableExpression vr(begin, type, name.name, nullptr);
                    if (!resolveOperator(scope, ast, DeclarationId(DeclarationId::Category::OperatorAssignment), vr, *init).first)
                        throwError(decl, "type mismatch in initialization");
                }
            }

            // Create a new variable
            scope.defineVariable(name.name, type, !init, !init);
            return {make_unique<VariableStatement>(begin, name.name, type, move(init)), ControlFlow::Normal};
        }
        case AST::Namespace:
            throwError(begin, "namespace definition not allowed here");
        case AST::Class:
            throwError(begin, "local classes not implemented yet"); // TODO
        case AST::Using:
        case AST::UsingDecltype:
            throwError(begin, "local using not implemented yet"); // TODO
        case AST::Template:
            throwError(begin, "templates not implemented yet"); // TODO
        default: throwError(begin, "invalid AST");
    }
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeCompoundStatement(Scope& scope, const AST* ast)
// Analyze a compound statement
{
    auto begin = mapping.getBegin(ast->getRange());
    Scope innerScope(scope);
    vector<unique_ptr<Statement>> statements;
    ControlFlow state = ControlFlow::Normal;
    for (auto s : ASTList(ast->getAnyOrNull(0))) {
        auto res = analyzeStatement(innerScope, s);
        if (state == ControlFlow::Normal) state = res.state;
        statements.push_back(move(res.statement));
    }
    auto end = mapping.getEnd(ast->getRange());

    innerScope.resolve(state);

    return {make_unique<CompoundStatement>(begin, end, move(statements)), state};
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeReturnStatement(Scope& scope, const AST* ast)
// Analyze a return statement
{
    auto fs = scope.getCurrentFunction();
    if (!fs)
        throwError(ast, "return statements are only allowed within functions");
    for (auto& v : fs->out) {
        if (!v.second->initialized)
            throwError(ast, "out parameter " + v.first + " must be initialized");
    }
    auto ft = fs->functionType;
    auto begin = mapping.getBegin(ast->getRange());
    unique_ptr<Expression> arg;
    if (ft->returnValues.empty()) {
        if (ast->getAnyOrNull(0))
            throwError(ast, "cannot return a value in function returning void");
    } else if ((ft->returnValues.size() == 1) && (ft->returnValues[0].first.empty())) {
        if (!ast->getAnyOrNull(0))
            throwError(ast, "cannot return a value in function returning void");
        arg = analyzeExpression(scope, ast->getAny(0), ft->returnValues[0].second);
        enforceConvertible(ast->getAny(0), arg, ft->returnValues[0].second);
    } else {
        // TODO check that return values have been assigned
        if (ast->getAny(0))
            throwError(ast, "return values are passed via named arguments in this function");
    }

    return {make_unique<ReturnStatement>(begin, move(arg)), ControlFlow::Returns};
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeSelectionStatement(Scope& scope, const AST* ast)
// Analyze a selection statement
{
    // Analyze the condition
    auto begin = mapping.getBegin(ast->getRange());
    auto cond = analyzeExpression(scope, ast->getAny(1));
    enforceConvertible(ast->getAny(1), cond, Type::getBool(*program), true);

    // A constexpr if?
    if (ast->getAnyOrNull(0)) {
        auto cv = deriveConstexpr(*cond);
        if (cv->getText() == "true") {
            return analyzeStatement(scope, ast->getAny(2));
        } else if (cv->getText() == "false") {
            if (ast->getAnyOrNull(3))
                return analyzeStatement(scope, ast->getAny(3));
            return {make_unique<CompoundStatement>(mapping.getBegin(ast->getRange()), mapping.getEnd(ast->getRange()), vector<unique_ptr<Statement>>()), ControlFlow::Normal};
        }
        throwError(ast, "constexpr not supported yet");
    }

    // A simple if?
    if (!ast->getAnyOrNull(3)) {
        Scope innerScope(scope);
        auto thenBranch = analyzeStatement(innerScope, ast->getAny(2));
        if ((thenBranch.state == ControlFlow::Normal) && !innerScope.getInitializedVars().empty())
            throwError(innerScope.getInitializedVars().front().ast, "variable might be uninitialized after 'if'");
        innerScope.resolve(thenBranch.state);
        return {move(thenBranch.statement), ControlFlow::Normal};
    }

    // An if with two branches
    Scope thenScope(scope);
    auto thenBranch = analyzeStatement(thenScope, ast->getAny(2));
    for (auto& i : thenScope.getInitializedVars()) i.var->initialized = false;
    Scope elseScope(scope);
    auto elseBranch = analyzeStatement(elseScope, ast->getAny(3));
    ControlFlow state = ControlFlow::Normal;
    if ((thenBranch.state == ControlFlow::Normal) && (elseBranch.state == ControlFlow::Normal)) {
        for (auto& i : thenScope.getInitializedVars())
            if (!i.var->initialized) throwError(i.ast, "variable is initialized in one branch but not the other");
        if (elseScope.getInitializedVars().size() != thenScope.getInitializedVars().size()) {
            // At least one variable is missing, find it for error reporting
            unordered_set<Scope::Var*> init1;
            for (auto& i : thenScope.getInitializedVars()) init1.insert(i.var);
            for (auto& i : elseScope.getInitializedVars())
                if (!init1.count(i.var))
                    throwError(i.ast, "variable is initialized in one branch but not the other");
        }
        // Pretend that the then branch terminated to avoid double bookkeeping
        thenScope.resolve(ControlFlow::Returns);
        for (auto& i : elseScope.getInitializedVars()) i.var->initialized = true;
        elseScope.resolve(ControlFlow::Normal);
    } else if (thenBranch.state == ControlFlow::Normal) {
        elseScope.resolve(elseBranch.state);
        for (auto& i : thenScope.getInitializedVars()) i.var->initialized = true;
        thenScope.resolve(thenBranch.state);
    } else if (elseBranch.state == ControlFlow::Normal) {
        thenScope.resolve(thenBranch.state);
        for (auto& i : elseScope.getInitializedVars()) i.var->initialized = true;
        elseScope.resolve(ControlFlow::Normal);
    } else {
        thenScope.resolve(thenBranch.state);
        elseScope.resolve(elseBranch.state);
        if (thenBranch.state == elseBranch.state) {
            state = thenBranch.state;
        } else {
            state = ControlFlow::ReturnsOrThrows;
        }
    }

    return {make_unique<SelectionStatement>(begin, move(cond), move(thenBranch.statement), move(elseBranch.statement)), state};
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeExpressionStatement(Scope& scope, const AST* ast)
// Analyze an expression statement
{
    auto begin = mapping.getBegin(ast->getRange());
    unique_ptr<Expression> arg = analyzeExpression(scope, ast->getAny(0));
    if (!arg) return {};

    return {make_unique<ExpressionStatement>(begin, move(arg)), ControlFlow::Normal};
}
//---------------------------------------------------------------------------
SemanticAnalysis::StatementResult SemanticAnalysis::analyzeStatement(Scope& scope, const AST* ast)
// Analyze a statement
{
    switch (ast->getType()) {
        case AST::DeclarationStatement: return analyzeDeclarationStatement(scope, ast);
        case AST::CompoundStatement: return analyzeCompoundStatement(scope, ast);
        case AST::ReturnStatement: return analyzeReturnStatement(scope, ast);
        case AST::SelectionStatement: return analyzeSelectionStatement(scope, ast);
        case AST::WhileStatement: throwError(ast, "while_statement not implemented yet"); // TODO
        case AST::DoWhileStatement: throwError(ast, "do_while_statement not implemented yet"); // TODO
        case AST::ForStatement: throwError(ast, "for_statement not implemented yet"); // TODO
        case AST::InspectExpression: throwError(ast, "inspect_statment not implemented yet"); // TODO
        case AST::ExpressionStatement: return analyzeExpressionStatement(scope, ast);
        default: throwError(ast, "invalid AST");
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
    if ((ast->getType() == AST::TypeModifier) && (ast->getSubType<ast::TypeModifier>() == ast::TypeModifier::Const) && isConst)
        throwError(ast, "const qualifier most only appear once");

    // Interpret the type
    switch (ast->getType()) {
        case AST::UnqualifiedId:
        case AST::QualifiedId: {
            auto decl = (ast->getType() == AST::UnqualifiedId) ? resolveUnqualifiedId(scope, ast) : resolveQualifiedId(scope, ast);
            if (!decl) return nullptr;
            if (!decl->isType())
                throwError(ast, "invalid type expression");
            return decl->getCorrespondingType()->withConst(isConst);
        }
        case AST::FundamentalType: {
            switch (ast->getSubType<ast::FundamentalType>()) {
                case ast::FundamentalType::Void: return Type::getVoid(*program)->withConst(isConst);
                case ast::FundamentalType::Char: {
                    // char can be modified by signed and unsigned
                    bool isSigned = false, isUnsigned = false;
                    for (auto m : List<AST::FundamentalTypeModifier>(ast->getOrNull(0, AST::FundamentalTypeModifierList))) {
                        if (isSigned || isUnsigned)
                            throwError(m, "invalid type modifier");
                        switch (m->getSubType<ast::FundamentalTypeModifier>()) {
                            case ast::FundamentalTypeModifier::Signed: isSigned = true; break;
                            case ast::FundamentalTypeModifier::Unsigned: isUnsigned = true; break;
                            case ast::FundamentalTypeModifier::Long: throwError(m, "'long' is not valid for char types");
                            case ast::FundamentalTypeModifier::Short: throwError(m, "'short' is not valid for char types");
                        }
                    }
                    // by default we assume char is signed. Should we make this platform specific? Could also be handled when writing C++1 out
                    return (isUnsigned ? Type::getUnsignedChar(*program) : Type::getChar(*program))->withConst(isConst);
                }
                case ast::FundamentalType::Char8: return Type::getChar8(*program)->withConst(isConst);
                case ast::FundamentalType::Char16: return Type::getChar16(*program)->withConst(isConst);
                case ast::FundamentalType::Char32: return Type::getChar32(*program)->withConst(isConst);
                case ast::FundamentalType::WChar: return Type::getWChar(*program)->withConst(isConst);
                case ast::FundamentalType::Bool: return Type::getBool(*program)->withConst(isConst);
                case ast::FundamentalType::Float: return Type::getFloat(*program)->withConst(isConst);
                case ast::FundamentalType::Double: return Type::getDouble(*program)->withConst(isConst);
                case ast::FundamentalType::LongDouble: return Type::getLongDouble(*program)->withConst(isConst);
                case ast::FundamentalType::Int: {
                    // Int can be modified by short/long/long long/signed/unsigned, in any order
                    bool isSigned = false, isUnsigned = false;
                    bool isShort = false;
                    unsigned isLong = 0;
                    auto invalidModifier = [this](const AST* ast) {
                        throwError(ast, "invalid type modifier");
                    };
                    for (auto m : List<AST::FundamentalTypeModifier>(ast->getOrNull(0, AST::FundamentalTypeModifierList))) {
                        switch (m->getSubType<ast::FundamentalTypeModifier>()) {
                            case ast::FundamentalTypeModifier::Signed:
                                if (isSigned || isUnsigned) invalidModifier(m);
                                isSigned = true;
                                break;
                            case ast::FundamentalTypeModifier::Unsigned:
                                if (isSigned || isUnsigned) invalidModifier(m);
                                isUnsigned = true;
                                break;
                            case ast::FundamentalTypeModifier::Long:
                                if (isShort || (isLong > 1)) invalidModifier(m);
                                ++isLong;
                                break;
                            case ast::FundamentalTypeModifier::Short:
                                if (isShort || isLong) invalidModifier(m);
                                isShort = true;
                                break;
                        }
                    }
                    if (isUnsigned) {
                        if (isShort) return Type::getUnsignedShort(*program)->withConst(isConst);
                        if (isLong > 1) return Type::getUnsignedLongLong(*program)->withConst(isConst);
                        if (isLong) return Type::getUnsignedLong(*program)->withConst(isConst);
                        return Type::getUnsignedInt(*program)->withConst(isConst);
                    } else {
                        if (isShort) return Type::getShort(*program)->withConst(isConst);
                        if (isLong > 1) return Type::getLongLong(*program)->withConst(isConst);
                        if (isLong) return Type::getLong(*program)->withConst(isConst);
                        return Type::getInt(*program)->withConst(isConst);
                    }
                }
            }
            return nullptr;
        }
        default: throwError(ast, "invalid AST");
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
void SemanticAnalysis::analyzeUnnamedDeclaration(Scope& scope, const AST* ast, const Type** type, unique_ptr<Expression>* value)
// Analyze an unnamed declaration
{
    auto statement = ast->getAnyOrNull(1);
    if (ast->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function) {
        // TODO
        throwError(ast, "nested function declarations not supported yet");
    } else {
        *type = nullptr;
        if (ast->getAny(0)) {
            *type = analyzeTypeIdExpression(scope, ast->getAny(0));
        } else if (!statement) {
            throwError(ast, "a deduced type must have an = initializer");
        }
        value->reset();
        if (statement) {
            *value = analyzeExpression(scope, statement, *type);
        }
    }
}
//---------------------------------------------------------------------------
const FunctionType* SemanticAnalysis::analyzeFunctionType(Scope& scope, const AST* ast, vector<unique_ptr<Expression>>* defaultArguments, unsigned* defaultArgumentsOffset, unsigned* declarationFlags)
// Analyze a function type declaration
{
    auto parameterList = ast->get(0, AST::ParameterDeclarationList);
    bool throwsSpecifier = ast->getAnyOrNull(1);
    auto returnList = ast->getOrNull(2, AST::ReturnList);
    auto modifierList = ast->getOrNull(3, AST::FunctionModifierList);
    auto contractList = ast->getOrNull(4, AST::ContractSeq);

    // Inspect the function modifiers
    unsigned slot = 0;
    auto classScope = dynamic_cast<Class*>(scope.getCurrentNamespace());
    FunctionType::ParameterDirection implicitDirection = FunctionType::ParameterDirection::Copy;
    unsigned flags = 0;
    auto bitMask = [](FunctionType::DeclarationFlags v) { return 1u << v; };
    auto bitMask2 = [](FunctionType::TypeFlags v) { return 1u << v; };
    auto hasMultipleBits = [](unsigned v) { return !!(v & (v - 1)); };
    for (auto m : List<AST::FunctionModifier>(modifierList)) {
        if ((!classScope) && (m->getSubType<ast::FunctionModifier>() != ast::FunctionModifier::Static))
            throwError(m, "modifier is allowed for class methods");
        FunctionType::ParameterDirection newDirection = FunctionType::ParameterDirection::Copy;
        unsigned newFlags = ~0u;
        switch (m->getSubType<ast::FunctionModifier>()) {
            case ast::FunctionModifier::In: newDirection = FunctionType::ParameterDirection::In; break;
            case ast::FunctionModifier::Out: newDirection = FunctionType::ParameterDirection::Out; break;
            case ast::FunctionModifier::Inout: newDirection = FunctionType::ParameterDirection::Inout; break;
            case ast::FunctionModifier::Move: newDirection = FunctionType::ParameterDirection::Move; break;
            case ast::FunctionModifier::Forward: newDirection = FunctionType::ParameterDirection::Forward; break;
            case ast::FunctionModifier::Static: newFlags = FunctionType::Static; break;
            case ast::FunctionModifier::Virtual: newFlags = FunctionType::Virtual; break;
            case ast::FunctionModifier::Abstract: newFlags = FunctionType::Abstract; break;
            case ast::FunctionModifier::Override: newFlags = FunctionType::Override; break;
            case ast::FunctionModifier::Final: newFlags = FunctionType::Final; break;
            case ast::FunctionModifier::Defaulted: newFlags = FunctionType::Defaulted; break;
            case ast::FunctionModifier::Deleted: newFlags = FunctionType::Deleted; break;
        }
        if (newDirection != FunctionType::ParameterDirection::Copy) {
            if (implicitDirection != FunctionType::ParameterDirection::Copy)
                throwError(m, "conflicting modifier");
            implicitDirection = newDirection;
        }
        if (~newFlags) {
            newFlags = 1 << newFlags;
            if (flags & newFlags)
                throwError(m, "conflicting modifier");
            flags |= newFlags;
        }
        if (((flags & bitMask(FunctionType::Static)) && (hasMultipleBits(flags) || (implicitDirection != FunctionType::ParameterDirection::Copy))) | (hasMultipleBits(flags & (bitMask(FunctionType::Virtual) | bitMask(FunctionType::Abstract) | bitMask(FunctionType::Override) | bitMask(FunctionType::Final)))))
            throwError(m, "conflicting modifier");
    }
    if (implicitDirection == FunctionType::ParameterDirection::Copy) implicitDirection = FunctionType::ParameterDirection::In;

    // Add implicit this parameter
    vector<FunctionType::Parameter> parameter;
    if (classScope && (!(flags & bitMask(FunctionType::Static)))) {
        FunctionType::Parameter pa;
        pa.name = "this";
        pa.type = classScope->getType();
        pa.direction = implicitDirection;
        parameter.push_back(move(pa));
        ++slot;
    }

    // Inspect all parameters
    if (defaultArguments) {
        defaultArguments->clear();
        *defaultArgumentsOffset = 0;
    }
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
        auto d = p->get(1, AST::Declaration);
        unique_ptr<Expression> defaultArgument;
        pa.name = extractIdentifier(d->getAny(0));
        analyzeUnnamedDeclaration(scope, d->get(1, AST::UnnamedDeclaration), &pa.type, &defaultArgument);
        if (defaultArguments) {
            if (defaultArgument) {
                if (defaultArguments->empty()) *defaultArgumentsOffset = slot;
                defaultArguments->push_back(move(defaultArgument));
            } else if (!defaultArguments->empty()) {
                throwError(p, "default argument missing");
            }
        }
        parameter.push_back(move(pa));
        ++slot;
    }

    // Handle the return type
    vector<pair<string, const Type*>> returnTypes;
    unsigned typeFlags = throwsSpecifier ? bitMask2(FunctionType::Throws) : 0;
    if (returnList) {
        switch (returnList->getSubType<ast::ReturnList>()) {
            case ast::ReturnList::Single:
            case ast::ReturnList::SingleRef: {
                bool isRef = returnList->getSubType<ast::ReturnList>() == ast::ReturnList::SingleRef;
                auto rt = analyzeIdExpression(scope, returnList->getAny(0));
                if (!isRef) {
                    rt = rt->getAsNonConst();
                } else {
                    typeFlags |= bitMask2(FunctionType::ReturnsRef);
                }
                returnTypes.emplace_back(""sv, rt);
                break;
            }
            case ast::ReturnList::Multiple: {
                auto pdl = returnList->get(0, AST::ParameterDeclarationList);
                for (auto r : List<AST::ParameterDeclaration>(pdl->getOrNull(0, AST::ParameterDeclarationSeq))) {
                    if (r->getAny(0))
                        throwError(r->getAny(0), "direction modifier not allowed in return list");
                    auto d = ast->get(1, AST::Declaration);
                    auto name = extractIdentifier(d->getAny(0));
                    const Type* type;
                    unique_ptr<Expression> defaultArgument;
                    analyzeUnnamedDeclaration(scope, d->get(1, AST::UnnamedDeclaration), &type, &defaultArgument);
                    if (defaultArgument)
                        throwError(d, "return value cannot have default values");
                    returnTypes.emplace_back(name, type);
                }
            }
        }
    }

    // Contracts are not supported yet
    if (contractList)
        // TODO
        throwError(contractList, "contracts not implemented yet");

    if (declarationFlags) *declarationFlags = flags;
    return FunctionType::get(*program, move(parameter), move(returnTypes), typeFlags);
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeDeclaration(Scope& scope, const AST* declaration)
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
            throwError(declaration, "duplicate definition");
    } else {
        if (isFunction) {
            decl = scope.getCurrentNamespace()->addDeclaration(make_unique<FunctionDeclaration>(loc, move(name), scope.getCurrentNamespace()));
        } else {
            if (!details->getAnyOrNull(0)) throwError(declaration, "type required");
            auto type = analyzeTypeIdExpression(scope, details->getAnyOrNull(0));
            decl = scope.getCurrentNamespace()->addDeclaration(make_unique<VariableDeclaration>(loc, move(name), scope.getCurrentNamespace(), type));
        }
    }

    // Analyze the signature
    unsigned slot = 0;
    if (isFunction) {
        auto fdecl = static_cast<FunctionDeclaration*>(decl);
        vector<unique_ptr<Expression>> defaultArguments;
        unsigned defaultArgumentsOffset, declarationFlags;
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), &defaultArguments, &defaultArgumentsOffset, &declarationFlags);
        auto existing = fdecl->findFunctionOverload(funcType);
        if (existing) {
            if (existing->type == funcType)
                throwError(declaration, "function overload with that signature already exists");
            throwError(declaration, "function overload with ambiguous signature already exists");
        }
        slot = fdecl->addFunctionOverload(loc, funcType, move(defaultArguments), defaultArgumentsOffset);
    }
    if (!inStdlib) program->trackSourceOrder(decl, slot);
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeDefinition(Scope& scope, const AST* declaration)
// Analyze a definition
{
    auto name = extractDeclarationId(declaration->getAny(0));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (isFunction) {
        if (inStdlib && (!details->getAnyOrNull(1))) return;

        auto fdecl = static_cast<FunctionDeclaration*>(decl);
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), nullptr, nullptr, nullptr);
        auto overload = fdecl->findFunctionOverload(funcType);

        FunctionScope fs(overload->type, {});
        Scope innerScope(scope);
        innerScope.setCurrentFunction(&fs);
        for (auto& p : funcType->getParameters()) {
            if (innerScope.definesVariable(p.name))
                throwError(declaration, "duplicate definition of " + p.name);
            auto type = p.type;
            if (p.direction == FunctionType::ParameterDirection::In) type = type->getAsConst();
            auto var = innerScope.defineVariable(p.name, type, p.direction == FunctionType::ParameterDirection::Out, p.direction == FunctionType::ParameterDirection::Out);
            if (p.direction == FunctionType::ParameterDirection::Out) fs.out.push_back({p.name, var});
        }
        auto res = analyzeStatement(innerScope, details->getAny(1));
        overload->statement = move(res.statement);
        if ((res.state == ControlFlow::Normal) && (funcType->returnValues.size() == 1))
            throwError(declaration, "function must return a value");
        innerScope.resolve(ControlFlow::Returns);
        for (auto& v : fs.out) {
            if (!v.second->initialized)
                throwError(declaration, "out parameter " + v.first + " must be initialized");
        }
    } else {
        // Declaration only?
        if (!details->getAnyOrNull(1)) {
            if (inStdlib) return;
            throwError(declaration, "a global declaration must be initialized");
        }

        // TODO
        throwError(declaration, "variable declarations not implemented yet");
    }
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeNamespace(Scope& scope, const AST* ast, Phase phase)
// Analyze a namespace
{
    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<NamespaceDeclaration>(mapping.getBegin(ast->getRange()), name, scope.getCurrentNamespace()));
    } else if (decl->getCategory() != Declaration::Category::Namespace) {
        throwError(ast, "duplicate definition");
    }
    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);

    {
        Scope innerScope(scope);
        innerScope.setCurrentNamespace(static_cast<NamespaceDeclaration*>(decl)->getNamespace());
        analyzeDeclarations(innerScope, ast->getAnyOrNull(1), phase);
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 1);
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeClass(Scope& scope, const AST* ast, Phase phase)
// Analyze a class definition
{
    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<ClassDeclaration>(mapping.getBegin(ast->getRange()), name, scope.getCurrentNamespace(), program.get()));
    } else if (phase == Phase::Declarations) {
        throwError(ast, "duplicate definition");
    }
    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);

    if (ast->getAnyOrNull(1))
        // TODO
        throwError(ast, "inheritance not implemented yet");

    {
        Scope innerScope(scope);
        innerScope.setCurrentNamespace(static_cast<ClassDeclaration*>(decl)->getClass());
        analyzeDeclarations(innerScope, ast->getAnyOrNull(2), phase);
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 1);
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeUsing(Scope& scope, const AST* ast, Phase phase, bool usingDecltype)
// Analyze a typedef
{
    if (phase != Phase::Declarations) return;

    DeclarationId name = string(extractIdentifier(ast->get(0, AST::Identifier)));
    const Type* type;
    if (!usingDecltype) {
        type = analyzeTypeIdExpression(scope, ast->getAny(1));
    } else {
        Scope innerScope(scope); // TODO set to ignore lifetime checks
        type = analyzeExpression(innerScope, ast->getAny(1))->getType();
    }
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (!decl) {
        decl = scope.getCurrentNamespace()->addDeclaration(make_unique<TypedefDeclaration>(mapping.getBegin(ast->getRange()), name, scope.getCurrentNamespace(), type));
    } else {
        throwError(ast, "duplicate definition");
    }

    if (phase == Phase::Declarations && !inStdlib) program->trackSourceOrder(decl, 0);
}
//---------------------------------------------------------------------------
void SemanticAnalysis::analyzeDeclarations(Scope& scope, const AST* declarations, Phase phase)
// Analyze a parse tree
{
    // Process all declarations
    for (auto d : ASTList(declarations))
        switch (d->getType()) {
            case AST::Type::Declaration:
                if (phase == Phase::Declarations)
                    analyzeDeclaration(scope, d);
                else
                    analyzeDefinition(scope, d);
                break;
            case AST::Type::Namespace:
                analyzeNamespace(scope, d, phase);
                break;
            case AST::Type::Class:
                analyzeClass(scope, d, phase);
                break;
            case AST::Type::Using:
            case AST::Type::UsingDecltype:
                analyzeUsing(scope, d, phase, d->getType() == AST::Type::UsingDecltype);
                break;
            case AST::Type::Template: throwError(d, "template not implemented yet");
            default: throwError(d, "invalid AST");
        }
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

    try {
        // Process all declarations
        analyzeDeclarations(scope, declarations, Phase::Declarations);

        // Now process all definitions
        analyzeDeclarations(scope, declarations, Phase::Definitions);
    } catch (const Error&) {
        return false;
    }

    return true;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
