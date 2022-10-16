#include "semana/SemanticAnalysis.hpp"
#include "parser/AST.hpp"
#include "program/Declaration.hpp"
#include "program/Expression.hpp"
#include "program/FunctionType.hpp"
#include "program/Module.hpp"
#include "program/Namespace.hpp"
#include "program/Program.hpp"
#include "program/Type.hpp"
#include <cassert>
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
std::unique_ptr<Expression> SemanticAnalysis::analyzeExpression(const AST* ast)
// Analyze and expression
{
    // TODO
    addError(ast, "analyzeExpression not implemented yet");
    return {};
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
bool SemanticAnalysis::analyzeUnnamedDeclaration(const AST* ast, const Type** type, unique_ptr<Expression>* value)
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
            if (!((*value = analyzeExpression(statement->getAny(0)))))
                return false;
        }
    }
    return true;
}
//---------------------------------------------------------------------------
const FunctionType* SemanticAnalysis::analyzeFunctionType(const AST* ast, vector<unique_ptr<Expression>>* defaultArguments, unsigned* defaultArgumentsOffset)
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
        if (!analyzeUnnamedDeclaration(d->get(1, AST::UnnamedDeclaration), &pa.type, &defaultArgument)) return nullptr;
        if (defaultArguments) {
            if (defaultArgument) {
                if (defaultArguments->empty()) *defaultArgumentsOffset = slot;
                defaultArguments->push_back(move(defaultArgument));
            } else if (!defaultArguments->empty()) {
                addError(p, "default argument missing");
            }
        }
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
                if (!analyzeUnnamedDeclaration(d->get(1, AST::UnnamedDeclaration), &type, &defaultArgument)) return nullptr;
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
bool SemanticAnalysis::analyzeDeclaration(const AST* declaration)
// Analyze a declaration
{
    auto name = extractIdentifier(declaration->get(0, AST::Identifier));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    // Check if the declaration already exists
    auto decl = currentNamespace->findDeclaration(name);
    if (decl) {
        // That is only permissive for functions
        if ((!isFunction) || (!decl->isFunction()))
            return addError(declaration, "duplicate definition");
    } else {
        decl = currentNamespace->addDeclaration(name, isFunction);
    }

    // Analyze the signature
    if (isFunction) {
        vector<unique_ptr<Expression>> defaultArguments;
        unsigned defaultArgumentsOffset;
        auto funcType = analyzeFunctionType(details->get(0, AST::FunctionType), &defaultArguments, &defaultArgumentsOffset);
        if (!funcType) return false;
        auto existing = decl->findFunctionOverload(funcType);
        if (existing) {
            if (existing->type == funcType)
                return addError(declaration, "function overload with that signature already exists");
            return addError(declaration, "function overload with ambiguous signature already exists");
        }
        decl->addFunctionOverload(funcType, move(defaultArguments), defaultArgumentsOffset);
    }

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyzeDefinition(const AST* declaration)
// Analyze a definition
{
    auto name = extractIdentifier(declaration->get(0, AST::Identifier));
    auto details = declaration->get(1, AST::UnnamedDeclaration);
    bool isFunction = details->getSubType<ast::UnnamedDeclaration>() == ast::UnnamedDeclaration::Function;

    auto decl = currentNamespace->findDeclaration(name);
    if (isFunction) {
        auto funcType = analyzeFunctionType(details->get(0, AST::FunctionType), nullptr, nullptr);
        auto overload = decl->findFunctionOverload(funcType);
    } else {
        // Declaration only?
        if (!details->getAnyOrNull(1))
            return addError(declaration, "a global declaration must be initialized");

        // TODO
    }

    return true;
}
//---------------------------------------------------------------------------
bool SemanticAnalysis::analyze(const AST* translationUnit)
// Analyze a parse tree
{
    assert(translationUnit && translationUnit->getType() == AST::TranslationUnit);
    auto declarations = translationUnit->getOrNull(0, AST::DeclarationSeq);

    currentNamespace = target->getGlobalNamespace();

    // Process all declarations
    for (auto d : ASTList(declarations))
        if (!analyzeDeclaration(d))
            return false;

    // Now process all definitions
    for (auto d : ASTList(declarations))
        if (!analyzeDefinition(d))
            return false;

    return true;
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
