#include "semana/SemanticAnalysis.hpp"
#include "parser/AST.hpp"
#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
#include "program/Module.hpp"
#include "program/Namespace.hpp"
#include "program/Program.hpp"
#include "program/Statement.hpp"
#include "program/Type.hpp"
#include "semana/Scope.hpp"
#include <cassert>
#include <limits>
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
bool SemanticAnalysis::enforceConvertible(const AST* loc, std::unique_ptr<Expression>& exp, const Type* target, [[maybe_unused]] bool explicitScope)
// Make sure an expression is convertible into a certain type
{
    auto ta = exp->getType()->getEffectiveType();
    auto tb = target->getEffectiveType();

    // Trivial conversion?
    if (ta == tb) return true;

    // Standard conversions
    if (tb->isPointerType()) {
        if (ta->isFundamentalType() && (static_cast<const FundamentalType*>(tb)->getId() == Type::FundamentalTypeId::NullptrType))
            return true;
        // TODO handle const
    } else if (tb->isFundamentalType()) {
        auto ib = static_cast<const FundamentalType*>(tb)->getId();
        if (ib == Type::FundamentalTypeId::Bool) {
            if (ta->isPointerType()) return true;
        }
        if (ta->isFundamentalType()) {
            auto ia = static_cast<const FundamentalType*>(ta)->getId();
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

//---------------------------------------------------------------------------
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpression([[maybe_unused]] Scope& scope, const AST* ast, [[maybe_unused]] const Type* typeHint)
// Analyze an expression
{
    switch (ast->getType()) {
        case AST::ExpressionList: addError(ast, "expression_list not implemented yet"); return {}; // TODO
        case AST::AssignmentExpression: addError(ast, "assignment_expression not implemented yet"); return {}; // TODO
        case AST::BinaryExpression: addError(ast, "binary_expression not implemented yet"); return {}; // TODO
        case AST::PrefixExpression: addError(ast, "prefix_expression not implemented yet"); return {}; // TODO
        case AST::PostfixExpression: addError(ast, "postfix_expression not implemented yet"); return {}; // TODO
        case AST::BracketExpression: addError(ast, "bracket_expression not implemented yet"); return {}; // TODO
        case AST::ParenExpression: addError(ast, "paren_expression not implemented yet"); return {}; // TODO
        case AST::DotExpression: addError(ast, "dot_expression not implemented yet"); return {}; // TODO
        case AST::InspectExpression: addError(ast, "inspect_expression not implemented yet"); return {}; // TODO
        case AST::Literal: return analyzeLiteral(ast);
        case AST::UnqualifiedId: addError(ast, "id_expression not implemented yet"); return {}; // TODO
        case AST::QualifiedId: addError(ast, "id_expression not implemented yet"); return {}; // TODO
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
// Analyze a compound statement
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
        case AST::ExpressionStatement: addError(ast, "expression_statement not implemented yet"); return {}; // TODO
        default: addError(ast, "invalid AST"); return {};
    }
}
//---------------------------------------------------------------------------
const Type* SemanticAnalysis::analyzeIdExpression(const AST* ast)
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
        case AST::UnqualifiedId: addError(ast, "analyzeIdExpression of unqualified_id not implemented yet"); return nullptr; // TODO
        case AST::QualifiedId: addError(ast, "analyzeIdExpression of qualified_id not implemented yet"); return nullptr; // TODO
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
                                return nullptr;
                            case ast::FundamentalTypeModifier::Short:
                                if (isShort || isLong) return invalidModifier(m);
                                isShort = true;
                                return nullptr;
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
const Type* SemanticAnalysis::analyzeTypeIdExpression(const AST* ast)
// Analyze an id-expression with optional pointer markers
{
    if (ast->getType() == AST::TypeModifier) {
        switch (ast->getSubType<ast::TypeModifier>()) {
            case ast::TypeModifier::Pointer: {
                auto t = analyzeTypeIdExpression(ast->getAny(0));
                return t ? t->getPointerTo() : nullptr;
            }
            case ast::TypeModifier::Const:
                return analyzeIdExpression(ast);
        }
        return nullptr;
    } else {
        return analyzeIdExpression(ast);
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
            if (!((*type = analyzeTypeIdExpression(ast->getAny(0)))))
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
        auto d = p->get(1, AST::Declaration);
        pa.name = extractIdentifier(d->getAny(0));
        unique_ptr<Expression> defaultArgument;
        if (!analyzeUnnamedDeclaration(scope, d->get(1, AST::UnnamedDeclaration), &pa.type, &defaultArgument)) return nullptr;
        if (defaultArguments) {
            if (defaultArgument) {
                if (defaultArguments->empty()) *defaultArgumentsOffset = slot;
                defaultArguments->push_back(move(defaultArgument));
            } else if (!defaultArguments->empty()) {
                addError(p, "default argument missing");
            }
        }
        parameter.push_back(move(pa));
        ++slot;
    }

    // Handle the return type
    vector<pair<string, const Type*>> returnTypes;
    if (returnList) {
        if (returnList->getSubType<ast::ReturnList>() == ast::ReturnList::Single) {
            auto rt = analyzeIdExpression(returnList->getAny(0));
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
    auto name = extractIdentifier(declaration->get(0, AST::Identifier));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    // Check if the declaration already exists
    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (decl) {
        // That is only permissive for functions
        if ((!isFunction) || (!decl->isFunction()))
            return addError(declaration, "duplicate definition");
    } else {
        decl = scope.getCurrentNamespace()->addDeclaration(loc, name, isFunction);
    }

    // Analyze the signature
    unsigned slot = 0;
    if (isFunction) {
        vector<unique_ptr<Expression>> defaultArguments;
        unsigned defaultArgumentsOffset;
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), &defaultArguments, &defaultArgumentsOffset);
        if (!funcType) return false;
        auto existing = decl->findFunctionOverload(funcType);
        if (existing) {
            if (existing->type == funcType)
                return addError(declaration, "function overload with that signature already exists");
            return addError(declaration, "function overload with ambiguous signature already exists");
        }
        slot = decl->addFunctionOverload(loc, funcType, move(defaultArguments), defaultArgumentsOffset);
    }
    program->trackSourceOrder(decl, slot);

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeDefinition(Scope& scope, const AST* declaration)
// Analyze a definition
{
    auto name = extractIdentifier(declaration->get(0, AST::Identifier));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    auto decl = scope.getCurrentNamespace()->findDeclaration(name);
    if (isFunction) {
        auto funcType = analyzeFunctionType(scope, details->get(0, AST::FunctionType), nullptr, nullptr);
        auto overload = decl->findFunctionOverload(funcType);

        FunctionScope fs(overload->type);
        Scope innerScope(scope);
        innerScope.setCurrentFunction(&fs);
        overload->statement = analyzeStatement(innerScope, details->getAny(1));
        return !!(overload->statement);
    } else {
        // Declaration only?
        if (!details->getAnyOrNull(1))
            return addError(declaration, "a global declaration must be initialized");

        // TODO
        addError(declaration, "variable declarations not implemented yet");
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
    for (auto d : ASTList(declarations))
        if (!analyzeDeclaration(scope, d))
            return false;

    // Now process all definitions
    for (auto d : ASTList(declarations))
        if (!analyzeDefinition(scope, d))
            return false;

    return true;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
