#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/IR/Instructions.h>
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "ast.hpp"
#include "type_inference.hpp"
#include "parser.hpp"
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <utility> 
  

template <class T>
class LLTable
{
    std::vector<std::map<std::string, T> *> *table;
    bool nameInScope(std::string name, std::map<std::string, T> *scope)
    {
        return (scope->find(name) != scope->end());
    }

public:
    LLTable() : table(new std::vector<std::map<std::string, T> *>())
    {
        openScope(); 
    }
    
    void insert(std::pair<std::string, T> entry)
    {
        (*table->back())[entry.first] = entry.second;
    }
    T operator[](std::string name)
    {
        for (auto it = table->rbegin(); it != table->rend(); it++)
        {
            if (nameInScope(name, *it))
                return (**it)[name];
        }
        return nullptr;
    }
    void openScope()
    {
        table->push_back(new std::map<std::string, T>());
    }
    void closeScope()
    {
        if (table->size() != 0)
        {
            std::map<std::string, T> *poppedScope = table->back();
            table->pop_back();
            delete poppedScope;
        }
    }
    int getCurrScope()
    {
        return (table->size() - 1);
    }
    int getScopeOf(std::string name)
    {
        int scope = getCurrScope();
        for (auto it = table->rbegin(); it != table->rend(); it++)
        {
            scope--;
            if (nameInScope(name, *it))
                break;
        }
        return scope;
    }
    ~LLTable() {}
};
LLTable<llvm::Value *> LLValues;


void openScopeOfAll()
{
    LLValues.openScope();
}
void closeScopeOfAll()
{
    LLValues.closeScope();
}


llvm::ConstantInt *AST::c1(bool b)
{
    return llvm::ConstantInt::get(TheContext, llvm::APInt(1, b, false));
}
llvm::ConstantInt *AST::c8(char c)
{
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
}
llvm::ConstantInt *AST::c32(int n)
{
    return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
}
llvm::ConstantInt *AST::c64(long int n)
{
    return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
}
llvm::Constant *AST::f80(long double d)
{
    return llvm::ConstantFP::get(flt, d);
}

llvm::Constant *AST::unitVal()
{
    return llvm::Constant::getNullValue(unitType);
}

static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                const std::string &VarName,
                                                llvm::Type *LLVMType)
{
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(LLVMType, nullptr, VarName.c_str());
}

std::map<std::string, llvm::Value *> declaredGlobalStrings;

bool stringDeclared(std::string s)
{
    return declaredGlobalStrings.find(s) != declaredGlobalStrings.end();
}

llvm::Value *getGlobalString(std::string s, llvm::IRBuilder<> Builder)
{
    if (!stringDeclared(s))
        declaredGlobalStrings[s] = Builder.CreateGlobalStringPtr(s);
    return declaredGlobalStrings[s];

}

llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(AST::TheContext);
llvm::Module *AST::TheModule;
llvm::legacy::FunctionPassManager *AST::TheFPM;

llvm::TargetMachine *AST::TargetMachine;

llvm::Type *AST::i1;
llvm::Type *AST::i8;
llvm::Type *AST::i32;
llvm::Type *AST::flt;
llvm::Type *AST::unitType;
llvm::Type *AST::machinePtrType;
llvm::Type *AST::arrCharType;

llvm::Function *AST::TheMalloc;
llvm::Function *AST::TheUncollectableMalloc;

void AST::start_compilation(const char *programName, bool optimize)
{
    TheModule = new llvm::Module(programName, TheContext);
    TheFPM = new llvm::legacy::FunctionPassManager(TheModule);
    if (optimize)
    {
        TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
        TheFPM->add(llvm::createInstructionCombiningPass());
        TheFPM->add(llvm::createReassociatePass());
        TheFPM->add(llvm::createGVNPass());
        TheFPM->add(llvm::createCFGSimplificationPass());
    }
    TheFPM->doInitialization();
    
	// emit object code init
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
    std::string Error;
    auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
    if (!Target)
    {
        llvm::errs() << Error;
        exit(1);
    }
    auto CPU = "generic";
    auto Features = "";
    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    TheModule->setDataLayout(TargetMachine->createDataLayout());
    TheModule->setTargetTriple(TargetTriple);
    i1 = type_bool->getLLVMType(TheModule);
    i8 = type_char->getLLVMType(TheModule);
    i32 = type_int->getLLVMType(TheModule);
    flt = type_float->getLLVMType(TheModule);
    unitType = type_unit->getLLVMType(TheModule);
    machinePtrType = llvm::Type::getIntNTy(TheContext, TheModule->getDataLayout().getMaxPointerSizeInBits());
    arrCharType = (new ArrayTypeGraph(1, new RefTypeGraph(type_char)))->getLLVMType(TheModule);
    std::vector<std::pair<std::string, llvm::Function *>> *libFunctions = genLibGlueLogic();
    for (auto &libFunc : *libFunctions)
    {
        LLValues.insert(libFunc);
    }

#ifdef LIBGC
    llvm::FunctionType *gcMallocType = llvm::FunctionType::get(i8->getPointerTo(), {machinePtrType}, false);
    TheMalloc = llvm::Function::Create(gcMallocType, llvm::Function::ExternalLinkage, "GC_malloc_atomic", TheModule);
    TheUncollectableMalloc = llvm::Function::Create(gcMallocType, llvm::Function::ExternalLinkage,
                           "GC_malloc_atomic_uncollectable", TheModule);
    llvm::FunctionType *gcFreeType = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8->getPointerTo()}, false);
    llvm::Function::Create(gcFreeType, llvm::Function::ExternalLinkage, "GC_free", TheModule);
#else
    TheMalloc = TheUncollectableMalloc = nullptr;
#endif // LIBGC
    llvm::FunctionType *main_type = llvm::FunctionType::get(i32, {}, false);
    llvm::Function *main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage, "main", TheModule);
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", main);
    Builder.SetInsertPoint(BB);
    compile();
    Builder.CreateRet(c32(0));

    bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
    if (bad)
    { 
        std::cerr << "The IR is bad!" << std::endl;
        TheModule->print(llvm::errs(), nullptr);
        std::exit(1);
    }
    TheFPM->run(*main);
}

void AST::printLLVMIR()
{
    TheModule->print(llvm::outs(), nullptr);
}

void AST::emitObjectCode(const char *filename)
{
    std::error_code EC;
    llvm::raw_fd_ostream dst(filename, EC, llvm::sys::fs::OF_None);

    if (EC)
    {
        llvm::errs() << "Could not open file: " << EC.message();
        exit(1);
    }

    llvm::legacy::PassManager pass;
    auto FileType = llvm::CGFT_ObjectFile;

    if (TargetMachine->addPassesToEmitFile(pass, dst, nullptr, FileType))
    {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        exit(1);
    }

    pass.run(*TheModule);
    dst.flush();
}
void AST::emitAssemblyCode(const char *filename)
{
	std::error_code EC;
    llvm::raw_fd_ostream dst(filename, EC, llvm::sys::fs::OF_None);

    if (EC)
    {
        llvm::errs() << "Could not open file: " << EC.message();
        exit(1);
    }

    llvm::legacy::PassManager pass;
    auto FileType = llvm::CGFT_AssemblyFile;

    if (TargetMachine->addPassesToEmitFile(pass, dst, nullptr, FileType))
    {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        exit(1);
    }

    pass.run(*TheModule);
    dst.flush();
}

void AST::emitLLVMIC(const char *filename){
	std::error_code EC;
    llvm::raw_fd_ostream dst(filename, EC, llvm::sys::fs::OF_None);

    if (EC)
    {
        llvm::errs() << "Could not open file: " << EC.message();
        exit(1);
    }
	TheModule->print(dst, nullptr);
}

void AST::emitAssemblyCode()
{
    llvm::legacy::PassManager pass;
    auto FileType = llvm::CGFT_AssemblyFile;

    if (TargetMachine->addPassesToEmitFile(pass, (llvm::raw_pwrite_stream &)llvm::outs(), nullptr, FileType))
    {
        llvm::errs() << "TargetMachine can't emit a file of this type";
    }
    pass.run(*TheModule);
    llvm::outs().flush();
}

llvm::Value *AST::getGlobalLiveValue() {
    return globalLiveValue;
}

llvm::Value* AST::updateGlobalValue(llvm::Value *newVal) {
    if (listOfFunctionsThatNeedSymbol.empty())
        return nullptr;
    if (!globalLiveValue) {
        auto initializer = llvm::ConstantAggregateZero::get(newVal->getType());
        globalLiveValue = new llvm::GlobalVariable( *TheModule, newVal->getType(), false,llvm::GlobalValue::InternalLinkage,initializer);
    }
    auto prevGlobal = Builder.CreateLoad(globalLiveValue, "reminder");
    Builder.CreateStore(newVal, globalLiveValue);
    return prevGlobal;
}


llvm::Value *AST::compile()
{
    return nullptr;
}

llvm::Value *Constr::compile()
{
    return nullptr;
}

llvm::Value *Tdef::compile()
{
    return nullptr;
}

llvm::Value *Constant::compile()
{
    llvm::Value *exprVal = expr->compile();
    exprVal->setName(id);
    LLValues.insert({id, exprVal});
    updateGlobalValue(exprVal);
    return nullptr;
}

llvm::StructType *Function::getEnvStructType()
{
    if (envStructType)  return envStructType;
    std::string envTypeName = getId() + ".env";
    ConstructorTypeGraph *utilGraph = new ConstructorTypeGraph(envTypeName);
    for (const auto &ext: external) {
        utilGraph->addField(ext.second->getTypeGraph());
    }
    envStructType = utilGraph->getLLVMType(TheModule);
    envStructType->setName(envTypeName);
    return envStructType;
}

void DefStmt::generateLLVMPrototype() {
    std::cerr << "generateLLVMPrototype called for DefStmt\n";
    exit(1);
}

void Function::generateLLVMPrototype()
{
    auto paramTypes = getTypeGraph()->getLLVMParamTypes(TheModule);
    auto resType = getTypeGraph()->getLLVMResultType(TheModule);
    paramTypes.push_back(getEnvStructType()->getPointerTo());
    auto newFuncType = llvm::FunctionType::get(resType, paramTypes, false);
    funcPrototype = llvm::Function::Create( newFuncType, llvm::Function::ExternalLinkage, id, TheModule );
}

llvm::Value *DefStmt::generateTrampoline() {
    std::cerr << "generateTrampoline() called for DefStmt\n";
    exit(1);
}

llvm::Value *Function::generateTrampoline()
{
    auto trampolineEnvMallocInst = llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(),machinePtrType, getEnvStructType(),
										llvm::ConstantExpr::getSizeOf(getEnvStructType()), nullptr, TheMalloc);
    auto trampolineEnvMalloc = Builder.Insert(trampolineEnvMallocInst, getId() + ".envmalloc");

    auto trampolineMallocSize = c32(16);
    auto trampolineMallocInst = llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(),
            machinePtrType, i8, llvm::ConstantExpr::getSizeOf(i8), trampolineMallocSize, TheMalloc);
    auto trampolineMalloc = Builder.Insert(trampolineMallocInst, getId() + ".trampmalloc");
    
    int i = 0;
    llvm::Value *currEnvLoc;
    AST *currDepNode;
    for (const auto &ext: external) {
        currEnvLoc = Builder.CreateGEP(trampolineEnvMalloc, {c32(0), c32(i)}, "tramp.currenvloc");
        currDepNode = ext.second->getNode();
        if (inf.deepReplace(ext.second->getTypeGraph())->isFunction()) {
            envBacklog.push_back({currDepNode, currEnvLoc});
        } 
		else {
            auto currDepVal = Builder.CreateLoad(currDepNode->getGlobalLiveValue(), "loadedglobaltmp");
            Builder.CreateStore(currDepVal, currEnvLoc, false);
        }
        i++;
    }
    llvm::Function *initTrampoline = llvm::Intrinsic::getDeclaration( TheModule, llvm::Intrinsic::init_trampoline);
    llvm::Function *adjustTrampoline = llvm::Intrinsic::getDeclaration( TheModule, llvm::Intrinsic::adjust_trampoline);
    auto *bitcastedFuncProto = Builder.CreatePointerCast( funcPrototype, i8->getPointerTo(), "castedfuncptrtmp"),
         *bitcastedEnvStruct = Builder.CreatePointerCast( trampolineEnvMalloc, i8->getPointerTo(), "castedfuncenvtmp");
        Builder.CreateCall(initTrampoline, {trampolineMalloc, bitcastedFuncProto, bitcastedEnvStruct});
    auto *adjustedTrampoline = Builder.CreateCall(adjustTrampoline, {trampolineMalloc}, "adjustedtrampoline");
    return Builder.CreatePointerCast(adjustedTrampoline, TG->getLLVMType(TheModule), "castedtrampoline");

}

void DefStmt::processEnvBacklog() {
    std::cerr << "processEnvBacklog() called for DefStmt \n";
    exit(1);
}
void Function::processEnvBacklog()
{
    for (const auto &pair: envBacklog) {
        Builder.CreateStore( Builder.CreateLoad(pair.first->getGlobalLiveValue()),pair.second, false);
    }
}

void DefStmt::generateBody() {
    std::cerr << "generateBody called for DefStmt\n";
    exit(1);
}
void Function::generateBody()
{    
	llvm::BasicBlock *prevBB = Builder.GetInsertBlock();
    openScopeOfAll();
    llvm::BasicBlock *newBB = llvm::BasicBlock::Create(TheContext, "entry", funcPrototype);
    Builder.SetInsertPoint(newBB);
    int i = 0;
    std::vector<std::pair<int,llvm::Value *>> previousGlobals;
    for (auto &arg : funcPrototype->args())
    {
        if ((long unsigned) i == par_list.size()) {
            arg.addAttr(llvm::Attribute::Nest);
            break; 
        }
        arg.setName(par_list[i]->getId());
        LLValues.insert({par_list[i]->getId(), &arg});
        previousGlobals.push_back({i, par_list[i]->updateGlobalValue(&arg)});
        i++;
    }
    i = 0;
    auto envStruct = funcPrototype->getArg(par_list.size());
    for (auto const &ext: external) {
        auto envField = Builder.CreateLoad(
            Builder.CreateGEP(envStruct, {c32(0), c32(i)}, "envfield")
        );
        LLValues.insert({ext.first, envField});
        i++;
    }
   
    auto retVal = expr->compile();
    for (auto const &pair: previousGlobals) {
        if (par_list[pair.first]->getGlobalLiveValue() == nullptr) continue;
        if (pair.second == nullptr) continue;
        Builder.CreateStore(pair.second, par_list[pair.first]->getGlobalLiveValue());
    }
    Builder.CreateRet(retVal);
    closeScopeOfAll();
    bool bad = llvm::verifyFunction(*funcPrototype, &llvm::errs());
    if (bad)
    {
        std::cerr << "Func verification failed for " << id << '\n';
        funcPrototype->print(llvm::errs(), nullptr);
        exit(1);
    }
    Builder.SetInsertPoint(prevBB);
    TheFPM->run(*funcPrototype);
}

llvm::Value *Function::compile()
{
    generateLLVMPrototype();
    generateBody();
    llvm::Value *newFunctionTrampoline = generateTrampoline();    
    processEnvBacklog();
    newFunctionTrampoline->setName(id);
    LLValues.insert({id, newFunctionTrampoline});
    updateGlobalValue(newFunctionTrampoline);
    return nullptr;
}
llvm::Value *Array::compile()
{
    int dimensions = this->get_dimensions();

    TypeGraph *containedTypeGraph = inf.deepReplace(T->get_TypeGraph());
    TypeGraph *arrayTypeGraph = new ArrayTypeGraph(dimensions, new RefTypeGraph(containedTypeGraph));
    llvm::Type *LLVMContainedType = containedTypeGraph->getLLVMType(TheModule);
    llvm::Type *LLVMType = arrayTypeGraph->getLLVMType(TheModule)->getPointerElementType();
    auto *LLVMMAllocInst = llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(), machinePtrType,
                                                        LLVMType, llvm::ConstantExpr::getSizeOf(LLVMType),
                                                        nullptr, TheMalloc,  "arr.def.malloc");
    llvm::Value *LLVMMAllocStruct = Builder.Insert(LLVMMAllocInst, "arr.def.mutable");

    llvm::ConstantInt *LLVMDimensions = c32(dimensions);

    std::vector<llvm::Value *> LLVMSize = {};
    for (auto e : expr_list)
    {
        LLVMSize.push_back(e->compile());
    }

    llvm::Value *LLVMArraySize = nullptr;
    for (auto size : LLVMSize)
    {
        if (!LLVMArraySize)
        {
            LLVMArraySize = size;
            continue;
        }

        LLVMArraySize = Builder.CreateMul(LLVMArraySize, size, "arr.def.multmp");
    }

    llvm::Instruction *LLVMMalloc =
        llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(), machinePtrType,LLVMContainedType, 
									 llvm::ConstantExpr::getSizeOf(LLVMContainedType),LLVMArraySize, TheMalloc, "arr.def.malloc");

    llvm::Value *LLVMAllocatedMemory = Builder.Insert(LLVMMalloc);
    llvm::Value *arrayPtrLoc = Builder.CreateGEP(LLVMMAllocStruct, {c32(0), c32(0)}, "arr.def.arrayptrloc");
    Builder.CreateStore(LLVMAllocatedMemory, arrayPtrLoc);

    llvm::Value *dimensionsLoc = Builder.CreateGEP(LLVMMAllocStruct, {c32(0), c32(1)}, "arr.def.dimloc");
    Builder.CreateStore(LLVMDimensions, dimensionsLoc);

    int step = 2;

    int sizeIndex;
    for (int i = 0; i < dimensions; i++)
    {
        sizeIndex = i + step;
        llvm::Value *sizeLoc = Builder.CreateGEP(LLVMMAllocStruct, {c32(0), c32(sizeIndex)}, "arr.def.sizeloc");
        Builder.CreateStore(LLVMSize[i], sizeLoc);
    }

    LLVMMAllocStruct->setName(id);
    LLValues.insert({id, LLVMMAllocStruct});
    updateGlobalValue(LLVMMAllocStruct);

    return nullptr;
}
llvm::Value *Variable::compile()
{
	
    llvm::Type *LLVMType = T->get_TypeGraph()->getLLVMType(TheModule);
    auto *LLVMMallocInst = llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(), machinePtrType, LLVMType, llvm::ConstantExpr::getSizeOf(LLVMType),
                                                        nullptr, TheMalloc, "var.def.malloc");
    llvm::Value *LLVMMAlloc = Builder.Insert(LLVMMallocInst, "var.def.mutable");
    LLVMMAlloc->setName(id);
    LLValues.insert({id, LLVMMAlloc});
    updateGlobalValue(LLVMMAlloc);

    return nullptr;
}
llvm::Value *Letdef::compile()
{
    if (!recursive)
    {
        for (auto def : def_list)
        {
            def->compile();
        }
    }
	
    else
    {
        for (auto &func : def_list)
        {   
            if (!func->isFunctionDefinition()) {
                std::cerr << "Recursive definition is NOT a function\n";
                exit(1);
            }
            func->generateLLVMPrototype();
        }

        for (auto &func : def_list) 
        {
            auto newFuncTrampoline = func->generateTrampoline();
            newFuncTrampoline->setName(func->getId());
            LLValues.insert({func->getId(), newFuncTrampoline});
            func->updateGlobalValue(newFuncTrampoline);
        }

        for (auto &func : def_list)
        {
            func->processEnvBacklog();
            func->generateBody();
        }
    }
    return nullptr;
}
llvm::Value *Typedef::compile()
{
    return nullptr;
}
llvm::Value *Program::compile()
{
    for (auto def : definition_list)
    {
        def->compile();
    }

    return nullptr;
}

llvm::Value *String_Const::compile()
{
    llvm::Value *strVal = getGlobalString(s, Builder);
    int size = s.size() + 1;
    llvm::Value *LLVMArraySize = c32(size);
    auto *LLVMMallocInst = llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(), machinePtrType,
                                                        arrCharType->getPointerElementType(),
                                                        llvm::ConstantExpr::getSizeOf(arrCharType->getPointerElementType()),nullptr,TheMalloc,"str.literal.malloc");
    llvm::Value *LLVMMallocStruct = Builder.Insert(LLVMMallocInst, "str.literal.mutable");

    // Allocate memory for string
    llvm::Instruction *LLVMMalloc =
        llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(),machinePtrType,i8,
			llvm::ConstantExpr::getSizeOf(i8),LLVMArraySize, TheMalloc,"stringalloc");

    llvm::Value *LLVMAllocatedMemory = Builder.Insert(LLVMMalloc);
    llvm::Value *arrayPtrLoc = Builder.CreateGEP(LLVMMallocStruct, {c32(0), c32(0)}, "stringptrloc");
    Builder.CreateStore(LLVMAllocatedMemory, arrayPtrLoc);

    llvm::Value *dimensionsLoc = Builder.CreateGEP(LLVMMallocStruct, {c32(0), c32(1)}, "dimensionsloc");
    Builder.CreateStore(c32(1), dimensionsLoc);

    llvm::Value *sizeLoc = Builder.CreateGEP(LLVMMallocStruct, {c32(0), c32(2)}, "sizeloc");
    Builder.CreateStore(LLVMArraySize, sizeLoc);

    Builder.CreateCall(TheModule->getFunction("strcpy"), {LLVMAllocatedMemory, strVal});

    return LLVMMallocStruct;
}
llvm::Value *Char_Const::compile()
{
    return c8(c);
}
llvm::Value *Bool_Const::compile()
{
    return c1(b);
}
llvm::Value *Float_Const::compile()
{
    return f80(d);
}
llvm::Value *Int_Const::compile()
{
    return c32(n);
}
llvm::Value *Unit::compile()
{
    return unitVal();
}

llvm::Value *Literal::LLVMCompare(llvm::Value *V)
{
    return nullptr;
}
llvm::Value *Char_Const::LLVMCompare(llvm::Value *V)
{
    llvm::Value *literalV = compile();
    return Builder.CreateICmpEQ(literalV, V, "literal.char.compare");
}
llvm::Value *Int_Const::LLVMCompare(llvm::Value *V)
{
    llvm::Value *literalV = compile();
    return Builder.CreateICmpEQ(literalV, V, "literal.int.compare");
}
llvm::Value *Float_Const::LLVMCompare(llvm::Value *V)
{
    llvm::Value *literalV = compile();
    return Builder.CreateFCmpOEQ(literalV, V, "literal.char.compare");
}

llvm::Value *BinOp::compile()
{
    if (op == T_andand || op == T_barbar)
    {
        auto *lhsLogicVal = lhs->compile();
        auto *shortCircuitExitBB = llvm::BasicBlock::Create( TheContext, "shortcircuit.exit", Builder.GetInsertBlock()->getParent()),
             *shortCircuitImpossibleBB = llvm::BasicBlock::Create( TheContext, "shortcircuit.impossible", Builder.GetInsertBlock()->getParent());
        llvm::IRBuilder<> TmpB(TheContext);
        TmpB.SetInsertPoint(shortCircuitExitBB);
        auto *phiCollector = TmpB.CreatePHI(i1, 2, "shortcircuit.restmp");

        if (op == T_andand)
        {
            auto *decider = Builder.CreateICmpEQ(lhsLogicVal, c1(false), "shortcircuit.andtmp");
            Builder.CreateCondBr(decider, shortCircuitExitBB, shortCircuitImpossibleBB);
            phiCollector->addIncoming(c1(false), Builder.GetInsertBlock());
            Builder.SetInsertPoint(shortCircuitImpossibleBB);
            auto *rhsLogicVal = rhs->compile();
            auto *operationRes = Builder.CreateAnd(lhsLogicVal, rhsLogicVal, "and.restmp");
            Builder.CreateBr(shortCircuitExitBB);
            phiCollector->addIncoming(operationRes, Builder.GetInsertBlock());
            Builder.SetInsertPoint(shortCircuitExitBB);
            return phiCollector;
        }
        else if (op == T_barbar)
        {
            auto *decider = Builder.CreateICmpEQ(lhsLogicVal, c1(true), "shortcircuit.ortmp");
            Builder.CreateCondBr(decider, shortCircuitExitBB, shortCircuitImpossibleBB);
            phiCollector->addIncoming(c1(true), Builder.GetInsertBlock());
            Builder.SetInsertPoint(shortCircuitImpossibleBB);
            auto *rhsLogicVal = rhs->compile();
            auto *operationRes = Builder.CreateOr(lhsLogicVal, rhsLogicVal, "or.restmp");
            Builder.CreateBr(shortCircuitExitBB);
            phiCollector->addIncoming(operationRes, Builder.GetInsertBlock());
            Builder.SetInsertPoint(shortCircuitExitBB);
            return phiCollector;
        }
    }
    else
    {
        auto lhsVal = lhs->compile(),
             rhsVal = rhs->compile();
        auto tempTypeGraph = inf.deepReplace(lhs->get_TypeGraph());

        switch (op)
        {
        case '+':
            return Builder.CreateAdd(lhsVal, rhsVal, "int.addtmp");
        case '-':
            return Builder.CreateSub(lhsVal, rhsVal, "int.subtmp");
        case '*':
            return Builder.CreateMul(lhsVal, rhsVal, "int.multmp");
        case '/':
            return Builder.CreateSDiv(lhsVal, rhsVal, "int.divtmp");
        case T_mod:
            return Builder.CreateSRem(lhsVal, rhsVal, "int.modtmp");

        case T_plusdot:
            return Builder.CreateFAdd(lhsVal, rhsVal, "float.addtmp");
        case T_mindot:
            return Builder.CreateFSub(lhsVal, rhsVal, "float.subtmp");
        case T_stardot:
            return Builder.CreateFMul(lhsVal, rhsVal, "float.multmp");
        case T_sldot:
            return Builder.CreateFDiv(lhsVal, rhsVal, "float.divtmp");
        case T_starstar:
        {
            
            return Builder.CreateCall(TheModule->getFunction("pow.custom"), {lhsVal, rhsVal}, "float.powtmp");
        }
        case T_barbar:
            return Builder.CreateOr({lhsVal, rhsVal});
        case T_andand:
            return Builder.CreateAnd({lhsVal, rhsVal});

        case '=':
        { 
            return equalityHelper(lhsVal, rhsVal, tempTypeGraph, true, Builder);
        }
        case T_lgr:
        { 
            return Builder.CreateNot(equalityHelper(lhsVal, rhsVal, tempTypeGraph, true, Builder));
        }
        case T_eqeq:
        { 
            return equalityHelper(lhsVal, rhsVal, tempTypeGraph, false, Builder);
        }
        case T_excleq:
        { 
            return Builder.CreateNot(equalityHelper(lhsVal, rhsVal, tempTypeGraph, false, Builder));
        }
        case '<':
        {
            if (tempTypeGraph->isFloat())
            {
                return Builder.CreateFCmpOLT(lhsVal, rhsVal, "float.cmplttmp");
            }
            else
            { 
                return Builder.CreateICmpSLT(lhsVal, rhsVal, "int.cmplttmp");
            }
        }
        case '>':
        {
            if (tempTypeGraph->isFloat())
            {
                return Builder.CreateFCmpOGT(lhsVal, rhsVal, "float.cmpgttmp");
            }
            else
            {  
                return Builder.CreateICmpSGT(lhsVal, rhsVal, "int.cmpgttmp");
            }
        }
        case T_leq:
        {
            if (tempTypeGraph->isFloat())
            {
                return Builder.CreateFCmpOLE(lhsVal, rhsVal, "float.cmpletmp");
            }
            else
            { 
                return Builder.CreateICmpSLE(lhsVal, rhsVal, "int.cmpletmp");
            }
        }
        case T_greq:
        {
            if (tempTypeGraph->isFloat())
            {
                return Builder.CreateFCmpOGE(lhsVal, rhsVal, "float.cmpgetmp");
            }
            else
            { 
                return Builder.CreateICmpSGE(lhsVal, rhsVal, "int.cmpgetmp");
            }
        }
        case T_coleq:
        {
            Builder.CreateStore(rhsVal, lhsVal);
            return unitVal();
        }
        case ';':
            return rhsVal;
        default:
            return nullptr;
        }
    }
    return nullptr;
}

llvm::Value *UnOp::compile()
{
    auto exprVal = expr->compile();

    switch (op)
    {
    case '+':
        return exprVal;
    case '-':
        return Builder.CreateSub(c32(0), exprVal, "int.negtmp");
    case T_plusdot:
        return exprVal;
    case T_mindot:
        return Builder.CreateFSub(llvm::ConstantFP::getZeroValueForNegation(flt), exprVal, "float.negtmp");
    case T_not:
        return Builder.CreateNot(exprVal, "bool.nottmp");
    case '!':
        return Builder.CreateLoad(exprVal, "ptr.dereftmp");
    case T_delete:
    {
#ifdef LIBGC
        llvm::Instruction *i8PtrCast = llvm::CastInst::CreatePointerCast(exprVal, i8->getPointerTo(), "delete.cast", Builder.GetInsertBlock());
        Builder.CreateCall(TheModule->getFunction("GC_free"), {i8PtrCast});
#else
        Builder.Insert(llvm::CallInst::CreateFree(exprVal, Builder.GetInsertBlock()));
#endif // LIBGC
        return unitVal();
    }
    default:
        return nullptr;
    }
}

llvm::Value *LetIn::compile()
{
    llvm::Value *retVal;
    openScopeOfAll();
    letdef->compile();
    retVal = expr->compile();
    closeScopeOfAll();
    return retVal;
}
llvm::Value *New::compile()
{
    TypeGraph *newTypeGraph = inf.deepReplace(TG);
    const std::string instrName = "new_" + newTypeGraph->makeTypeString2() + "_alloc";
    llvm::Type *newType = newTypeGraph->getContainedType()->getLLVMType(TheModule);

    llvm::Instruction *LLVMMalloc =
        llvm::CallInst::CreateMalloc(Builder.GetInsertBlock(),machinePtrType,newType,llvm::ConstantExpr::getSizeOf(newType),
									 nullptr,TheUncollectableMalloc,instrName);

    llvm::Value *LLVMAllocatedMemory = Builder.Insert(LLVMMalloc);

    return LLVMAllocatedMemory;
}
llvm::Value *While::compile()
{
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "whileloop");
    llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(TheContext, "whilebody");
    llvm::BasicBlock *FinishBB = llvm::BasicBlock::Create(TheContext, "whileend");

    Builder.CreateBr(LoopBB);

    TheFunction->getBasicBlockList().push_back(LoopBB);
    Builder.SetInsertPoint(LoopBB);

    llvm::Value *LLVMCond = cond->compile();

    Builder.CreateCondBr(LLVMCond, BodyBB, FinishBB);

    TheFunction->getBasicBlockList().push_back(BodyBB);
    Builder.SetInsertPoint(BodyBB);

    body->compile();
    Builder.CreateBr(LoopBB);

    TheFunction->getBasicBlockList().push_back(FinishBB);
    Builder.SetInsertPoint(FinishBB);
	return unitVal();
}
llvm::Value *For::compile()
{
    bool increment = (step == "to");
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *PreheaderBB = Builder.GetInsertBlock();

    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "forloop");
    llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(TheContext, "forbody");
    llvm::BasicBlock *FinishBB = llvm::BasicBlock::Create(TheContext, "forend");


    llvm::Value *StartV = start->compile();
    llvm::Value *FinishV = finish->compile();

    llvm::Value *StepV = increment ? c32(1) : c32(-1);

    openScopeOfAll();

    Builder.CreateBr(LoopBB);


    TheFunction->getBasicBlockList().push_back(LoopBB);
    Builder.SetInsertPoint(LoopBB);

    llvm::PHINode *LoopVariable = Builder.CreatePHI(i32, 2, id);
    LoopVariable->addIncoming(StartV, PreheaderBB);
    LoopVariable->setName(id);
    LLValues.insert({id, LoopVariable});
    updateGlobalValue(LoopVariable);

    llvm::Value *LLVMCond =increment ? Builder.CreateICmpSLE(LoopVariable, FinishV, "forloopchecklte")
									: Builder.CreateICmpSGE(LoopVariable, FinishV, "forlookcheckgte");
    Builder.CreateCondBr(LLVMCond, BodyBB, FinishBB);

    TheFunction->getBasicBlockList().push_back(BodyBB);
    Builder.SetInsertPoint(BodyBB);
    body->compile();

    llvm::Value *NextV = Builder.CreateAdd(LoopVariable, StepV, "forstep");

    LoopVariable->addIncoming(NextV, Builder.GetInsertBlock());
    Builder.CreateBr(LoopBB);

    TheFunction->getBasicBlockList().push_back(FinishBB);
    Builder.SetInsertPoint(FinishBB);

    closeScopeOfAll();
    return unitVal();
}
llvm::Value *If::compile()
{
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
    llvm::Type *LLVMIfReturnType = TG->getLLVMType(TheModule);
    llvm::Value *LLVMCond = cond->compile();
    LLVMCond = Builder.CreateICmpEQ(LLVMCond, c1(true), "ifcond");

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then");
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

    Builder.CreateCondBr(LLVMCond, ThenBB, ElseBB);
    TheFunction->getBasicBlockList().push_back(ThenBB);
    Builder.SetInsertPoint(ThenBB);
    llvm::Value *ThenV = body->compile();
    ThenBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);

    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    llvm::Value *ElseV = unitVal();
    if (else_body)
        ElseV = else_body->compile();
    ElseBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);

    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *retVal = Builder.CreatePHI(LLVMIfReturnType, 2, "ifretval");
    retVal->addIncoming(ThenV, ThenBB);
    retVal->addIncoming(ElseV, ElseBB);

    return retVal;
}
llvm::Value *Dim::compile()
{
    int step = 2;
    int selectedDim = dim->get_int() - 1;
    llvm::Value *LLVMPointerToStruct = LLValues[id];
    llvm::Value *LLVMSizeLoc = Builder.CreateGEP(LLVMPointerToStruct, {c32(0), c32(selectedDim + step)}, "dimsizeloc");
    return Builder.CreateLoad(LLVMSizeLoc);
}
llvm::Value *ConstantCall::compile()
{
    return LLValues[id];
}
llvm::Value *FunctionCall::compile()
{
    llvm::Value *tempFunc = LLValues[id]; 
    std::vector<llvm::Value *> argsGiven;
    for (auto &arg : expr_list)
    {
        argsGiven.push_back(arg->compile());
    }
    return Builder.CreateCall(tempFunc, argsGiven, "func.calltmp");
}
llvm::Value *ConstrCall::compile()
{
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    int constrIndex = constructorTypeGraph->getIndex();
    llvm::StructType *constrType = constructorTypeGraph->getLLVMType(TheModule);
    llvm::AllocaInst *LLVMContainedStructAlloca = CreateEntryBlockAlloca(TheFunction, "constructorstruct", constrType);
    llvm::StructType *customType = llvm::dyn_cast<llvm::StructType>(constructorTypeGraph->getCustomType()->getLLVMType(TheModule)->getPointerElementType());
	
    auto LLVMCustomStructMallocInst = llvm::CallInst::CreateMalloc(
        Builder.GetInsertBlock(),machinePtrType, customType,llvm::ConstantExpr::getSizeOf(customType),
        nullptr, TheMalloc, "customstruct.malloc");
    llvm::Value *LLVMCustomStructPtr = Builder.Insert(LLVMCustomStructMallocInst);

    llvm::Value *constrFieldLoc, *LLVMParam;
    for (int i = 0; i < (int)expr_list.size(); i++)
    {
        LLVMParam = expr_list[i]->compile();

        constrFieldLoc = Builder.CreateGEP(LLVMContainedStructAlloca, {c32(0), c32(i)}, "constrFieldLoc");
        Builder.CreateStore(LLVMParam, constrFieldLoc);
    }

    llvm::Value *enumLoc = Builder.CreateGEP(LLVMCustomStructPtr, {c32(0), c32(0)}, "customenumloc");
    Builder.CreateStore(c32(constrIndex), enumLoc);
    llvm::Type *customFieldTypePtr = customType->getTypeAtIndex(1)->getPointerTo();
    llvm::Instruction *LLVMCastStructPtr = llvm::CastInst::CreatePointerCast(LLVMContainedStructAlloca, customFieldTypePtr, "constructorbitcast", Builder.GetInsertBlock());
    llvm::Value *LLVMCastStruct = Builder.CreateLoad(LLVMCastStructPtr);
    llvm::Value *constructorLoc = Builder.CreateGEP(LLVMCustomStructPtr, {c32(0), c32(1)}, "customconstructorloc");
    Builder.CreateStore(LLVMCastStruct, constructorLoc);

    return LLVMCustomStructPtr;
}
llvm::Value *ArrayAcc::compile()
{
    std::vector<llvm::Value *> LLVMArrayIndices = {};
    for (auto e : expr_list)
    {
        LLVMArrayIndices.push_back(e->compile());
    }

    llvm::Value *LLVMArrayStruct = LLValues[id];
    std::vector<llvm::Value *> LLVMSize = {};
    llvm::Value *arrayPtrLoc = Builder.CreateGEP(LLVMArrayStruct, {c32(0), c32(0)}, "arr.acc.ptrloc");
    llvm::Value *LLVMArray = Builder.CreateLoad(arrayPtrLoc);

    int step = 2;

    llvm::Value *sizeLoc;
    int sizeIndex, dimensions = expr_list.size();
    for (int i = 0; i < dimensions; i++)
    {
        sizeIndex = i + step;
        sizeLoc = Builder.CreateGEP(LLVMArrayStruct, {c32(0), c32(sizeIndex)}, "arr.acc.sizeloc");
        LLVMSize.push_back(Builder.CreateLoad(sizeLoc));
    }

    llvm::BasicBlock *currentBB = llvm::BasicBlock::Create(TheContext, "boundcheck.init", Builder.GetInsertBlock()->getParent()),
                     *outOfBoundsBB = llvm::BasicBlock::Create(TheContext, "boundcheck.outofbounds", Builder.GetInsertBlock()->getParent());
    Builder.CreateBr(currentBB);
    for (int i = 0; i < dimensions; i++)
    {
        Builder.SetInsertPoint(currentBB);
        auto checkDimVal = 
            Builder.CreateICmpSLT(LLVMArrayIndices[i], LLVMSize[i], std::string("checkdim.") + std::to_string(i));
        auto nextBB = llvm::BasicBlock::Create(TheContext, "boundcheck.nextdim", Builder.GetInsertBlock()->getParent());
        Builder.CreateCondBr(checkDimVal, nextBB, outOfBoundsBB);
        currentBB = nextBB;
    }
    Builder.SetInsertPoint(outOfBoundsBB);
    Builder.CreateCall(TheModule->getFunction("writeString"), {getGlobalString("Runtime error: array index out of bounds", Builder)});
    Builder.CreateCall(TheModule->getFunction("_exit"), {c32(1)});
    Builder.CreateBr(outOfBoundsBB);

    Builder.SetInsertPoint(currentBB);

    llvm::Value *LLVMTemp = LLVMArrayIndices[dimensions - 1],
                *LLVMArrayLoc = LLVMTemp,
                *LLVMSuffixSizeMul = LLVMSize[dimensions - 1];
    for (int i = dimensions - 2; i >= 0; i--)
    {
        LLVMTemp = Builder.CreateMul(LLVMSuffixSizeMul, LLVMArrayIndices[i]);
        LLVMArrayLoc = Builder.CreateAdd(LLVMArrayLoc, LLVMTemp);
        if (i != 0)
        {
            LLVMSuffixSizeMul = Builder.CreateMul(LLVMSuffixSizeMul, LLVMSize[i]);
        }
    }

    return Builder.CreateGEP(LLVMArray, LLVMArrayLoc, "arr.acc.elemptr");
}


llvm::Value *Match::compile()
{
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
    llvm::Value *toMatchV = toMatch->compile();
    llvm::BasicBlock *FinishBB = llvm::BasicBlock::Create(TheContext, "match.finish");
    std::vector<llvm::BasicBlock *> ClauseBB = {};

    std::vector<llvm::Value *> ClauseV = {};

    llvm::BasicBlock *SuccessBB;
    llvm::BasicBlock *NextClauseBB = llvm::BasicBlock::Create(TheContext, "match.firstclause");
    Builder.CreateBr(NextClauseBB);
    for (int i = 0; i < (int)clause_list.size(); i++)
    {
        auto c = clause_list[i];
        TheFunction->getBasicBlockList().push_back(NextClauseBB);
        Builder.SetInsertPoint(NextClauseBB);

        bool isLastClause = (i == (int)clause_list.size() - 1);
        std::string NextClauseBBName = (isLastClause) ? "match.fail" : "match.nextclause";
        NextClauseBB = llvm::BasicBlock::Create(TheContext, NextClauseBBName);
        SuccessBB = llvm::BasicBlock::Create(TheContext, "match.success");

        openScopeOfAll();
        llvm::Value *tryToMatchV = c->tryToMatch(toMatchV, NextClauseBB);

        Builder.CreateCondBr(tryToMatchV, SuccessBB, NextClauseBB);

        TheFunction->getBasicBlockList().push_back(SuccessBB);
        Builder.SetInsertPoint(SuccessBB);
        ClauseV.push_back(c->compile());

        closeScopeOfAll();
        ClauseBB.push_back(Builder.GetInsertBlock());
        Builder.CreateBr(FinishBB);
    }

    TheFunction->getBasicBlockList().push_back(NextClauseBB);
    Builder.SetInsertPoint(NextClauseBB);
    Builder.CreateCall(TheModule->getFunction("writeString"), {getGlobalString("Runtime Error: No clause matches given expression\n", Builder)});
    Builder.CreateCall(TheModule->getFunction("_exit"), {c32(1)});
    Builder.CreateBr(NextClauseBB);

    TheFunction->getBasicBlockList().push_back(FinishBB);
    Builder.SetInsertPoint(FinishBB);
    llvm::Type *retType = TG->getLLVMType(TheModule);
    llvm::PHINode *retVal = Builder.CreatePHI(retType, clause_list.size(), "match.retval");

    for (int i = 0; i < (int)clause_list.size(); i++)
    {
        retVal->addIncoming(ClauseV[i], ClauseBB[i]);
    }
    return retVal;
}
llvm::Value *Clause::compile()
{
    return expr->compile();
}
llvm::Value *Clause::tryToMatch(llvm::Value *toMatchV, llvm::BasicBlock *NextClauseBB)
{
    pattern->set_toMatchV(toMatchV);
    pattern->set_NextClauseBB(NextClauseBB);
    return pattern->compile();
}

void Pattern::set_toMatchV(llvm::Value *v)
{
    toMatchV = v;
}
void Pattern::set_NextClauseBB(llvm::BasicBlock *b)
{
    NextClauseBB = b;
}
llvm::Value *Pattern_Const::compile()
{
    return literal->LLVMCompare(toMatchV);
}
llvm::Value *PatternId::compile()
{
    toMatchV->setName(id);
    LLValues.insert({id, toMatchV});
    updateGlobalValue(toMatchV);
    return c1(true);
}
llvm::Value *PatternConstr::compile()
{
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
	
    int index = constrTypeGraph->getIndex();
    llvm::Value *toMatchIndexLoc = Builder.CreateGEP(toMatchV, {c32(0), c32(0)});
    llvm::Value *toMatchIndex = Builder.CreateLoad(toMatchIndexLoc, "pattern.constr.loadindex");
    llvm::Value *indexCmp = Builder.CreateICmpEQ(c32(index), toMatchIndex);
    llvm::BasicBlock *SameConstrBB = llvm::BasicBlock::Create(TheContext, "pattern.constr.sameconstr");
    Builder.CreateCondBr(indexCmp, SameConstrBB, NextClauseBB);

    TheFunction->getBasicBlockList().push_back(SameConstrBB);
    Builder.SetInsertPoint(SameConstrBB);

    llvm::StructType *constrType = constrTypeGraph->getLLVMType(TheModule);
    llvm::Type *constrTypePtr = constrType->getPointerTo();
    llvm::Value *toMatchConstrStructLoc = Builder.CreateGEP(toMatchV, {c32(0), c32(1)});
    llvm::Value *LLVMCastStructPtr = llvm::CastInst::CreatePointerCast(toMatchConstrStructLoc, constrTypePtr, "pattern.constr.bitcast", Builder.GetInsertBlock());
    llvm::Value *canMatchFields = c1(true);

    for (int i = 0; i < (int)pattern_list.size(); i++)
    {
        auto p = pattern_list[i];

        llvm::Value *castStructFieldLoc = Builder.CreateGEP(LLVMCastStructPtr, {c32(0), c32(i)}, "pattern.constr.fieldloc");
        llvm::Value *tempV = Builder.CreateLoad(castStructFieldLoc, "pattern.constr.structfield");
        p->set_toMatchV(tempV);
        p->set_NextClauseBB(NextClauseBB);
        tempV = p->compile();
        canMatchFields = Builder.CreateAnd(canMatchFields, tempV);
    }

    return canMatchFields;
}

llvm::Function* AST::createFuncAdapterFromVoidToUnit(llvm::Function *voidFunc) {
    llvm::Type *retType = voidFunc->getReturnType();
    std::vector<llvm::Type *> paramTypes = {};
    if (retType->isVoidTy()) { retType = unitType; }
    for (auto &arg: voidFunc->args()) { 
        paramTypes.push_back(arg.getType());
    }
    if (paramTypes.empty()) { paramTypes.push_back(unitType); }
    
    llvm::FunctionType *wrapperFuncType = llvm::FunctionType::get(retType, paramTypes, false);
    llvm::Function *wrapperFunc =  llvm::Function::Create(wrapperFuncType, llvm::Function::InternalLinkage, 
                                  "to.unit." + voidFunc->getName(), TheModule);
    llvm::BasicBlock *wrapperFuncBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", wrapperFunc);
    
    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(wrapperFuncBB);
    std::vector<llvm::Value *> params = {};
    for (auto &arg: wrapperFunc->args()) {
        if (arg.getType() == unitType) 
            continue;
        params.push_back(&arg);
    }
    if (retType == unitType) {
        TmpB.CreateCall(voidFunc, params);
        TmpB.CreateRet(unitVal());
    } else {
        TmpB.CreateRet(TmpB.CreateCall(voidFunc, params, "to.unit.wrapper"));
    }
    TheFPM->run(*wrapperFunc);
    return wrapperFunc;
}

llvm::Function* AST::createFuncAdapterFromStringToCharArr(llvm::Function *stringFunc) {
    llvm::Type *i8ptr = i8->getPointerTo();
    llvm::Type *retType = stringFunc->getReturnType();
    std::vector<llvm::Type *> paramTypes = {};


    if (retType == i8ptr) { retType = arrCharType; }
    for (auto &arg: stringFunc->args()) {
        if (arg.getType() == i8ptr) {
            paramTypes.push_back(arrCharType);
        } else {
            paramTypes.push_back(arg.getType());
        }
    }
    llvm::FunctionType *wrapperFuncType = llvm::FunctionType::get(retType, paramTypes, false);
    llvm::Function *wrapperFunc =  llvm::Function::Create(wrapperFuncType, llvm::Function::InternalLinkage, 
                                  "to.chararr." + stringFunc->getName(), TheModule);
    llvm::BasicBlock *wrapperFuncBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", wrapperFunc);

    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(wrapperFuncBB);
    std::vector<llvm::Value *> params = {};
    for (auto &arg: wrapperFunc->args()) {
        if (arg.getType() == arrCharType) {
            llvm::Value *tmpInnerArr = TmpB.CreateGEP(&arg, {c32(0), c32(0)}, "arr.wrap.ptrloc");
            params.push_back(TmpB.CreateLoad(tmpInnerArr));
        } 
		else {
            params.push_back(&arg);
        }
    }
    llvm::Value *retValCandidate = TmpB.CreateCall(stringFunc, params, "to.arrchar.wrapper");

    if (retType == arrCharType) {
        auto *arrayOfCharMalloc =  llvm::CallInst::CreateMalloc(TmpB.GetInsertBlock(), machinePtrType, arrCharType->getPointerElementType(),
                                                             llvm::ConstantExpr::getSizeOf(arrCharType->getPointerElementType()),
                                                             nullptr, TheMalloc,"to.arrchar.retval");
        llvm::Value *arrayOfCharVal = TmpB.Insert(arrayOfCharMalloc);
        llvm::Value *arrayPtrLoc = TmpB.CreateGEP(arrayOfCharVal, {c32(0), c32(0)}, "to.arrchar.arrayptrloc");
        TmpB.CreateStore(retValCandidate, arrayPtrLoc);
        llvm::Value *dimLoc = TmpB.CreateGEP(arrayOfCharVal, {c32(0), c32(1)}, "to.arrchar.dimloc");
        TmpB.CreateStore(c32(1), dimLoc);
        llvm::Value *sizeLoc = TmpB.CreateGEP(arrayOfCharVal, {c32(0), c32(2)}, "to.arrchar.sizeloc");
        llvm::Value *size = TmpB.CreateCall(TheModule->getFunction("strlen"), {retValCandidate}, "to.arrchar.size");
        TmpB.CreateStore(TmpB.CreateAdd(size, c32(1)), sizeLoc);
        retValCandidate = arrayOfCharVal;
    }
    TmpB.CreateRet(retValCandidate);
    TheFPM->run(*wrapperFunc);
    return wrapperFunc;
}

llvm::Function* adaptReadString(llvm::Function *ReadString, llvm::Module *TheModule,
                                llvm::FunctionType *arrchar_to_unit, llvm::Value *unitVal,
                                llvm::Value *c32_0, llvm::Value *c32_1, llvm::Value *c32_2,
                                llvm::legacy::FunctionPassManager *TheFPM) {
    llvm::Function *readStringAdapted = llvm::Function::Create(arrchar_to_unit, llvm::Function::InternalLinkage,  "read_string", TheModule);
    llvm::BasicBlock *readStringAdaptedBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", readStringAdapted);
    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(readStringAdaptedBB);
    llvm::Value *readStringArrCharArg = readStringAdapted->getArg(0);
    llvm::Value *readStringStringLoc = TmpB.CreateGEP(readStringArrCharArg, {c32_0, c32_0}, "readString.strloc");
    llvm::Value *readStringSizeLoc = TmpB.CreateGEP(readStringArrCharArg, {c32_0, c32_2}, "readString.sizeloc");
    llvm::Value *readStringSize = TmpB.CreateSub(TmpB.CreateLoad(readStringSizeLoc), c32_1);
    llvm::Value *readStringString = TmpB.CreateLoad(readStringStringLoc);
    TmpB.CreateCall(ReadString, {readStringSize, readStringString});
    TmpB.CreateRet(unitVal);
    TheFPM->run(*readStringAdapted);
    return readStringAdapted;
}

llvm::Function *createIncrLibFunc(llvm::Module *TheModule, llvm::Type *unitType, llvm::Value *c32_1, llvm::Value *unitVal, llvm::legacy::FunctionPassManager *TheFPM) {
    llvm::FunctionType *intptr_to_unit = llvm::FunctionType::get(unitType, {llvm::Type::getInt32PtrTy(TheModule->getContext())}, false);
    llvm::Function *incrFunc =  llvm::Function::Create(intptr_to_unit, llvm::Function::InternalLinkage, "incr", TheModule);
    llvm::BasicBlock *incrFuncBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", incrFunc);
    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(incrFuncBB);
    llvm::Value *prevVal = TmpB.CreateLoad(incrFunc->getArg(0), "prevval");
    TmpB.CreateStore(TmpB.CreateAdd(prevVal, c32_1, "newval"), incrFunc->getArg(0));
    TmpB.CreateRet(unitVal);
    TheFPM->run(*incrFunc);
    return incrFunc;
}

llvm::Function *createDecrLibFunc(llvm::Module *TheModule, llvm::Type *unitType, llvm::Value *c32_1, llvm::Value *unitVal, llvm::legacy::FunctionPassManager *TheFPM) {
    llvm::FunctionType *intptr_to_unit = llvm::FunctionType::get(unitType, {llvm::Type::getInt32PtrTy(TheModule->getContext())}, false);
    llvm::Function *decrFunc = llvm::Function::Create(intptr_to_unit, llvm::Function::InternalLinkage, "decr", TheModule);
    llvm::BasicBlock *decrFuncBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", decrFunc);
    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(decrFuncBB);
    llvm::Value *prevVal = TmpB.CreateLoad(decrFunc->getArg(0), "prevval");
    TmpB.CreateStore(TmpB.CreateSub(prevVal, c32_1, "newval"), decrFunc->getArg(0));
    TmpB.CreateRet(unitVal);
    TheFPM->run(*decrFunc);
    return decrFunc;
}

llvm::Function *createFloatOfIntLibFunc(llvm::Module *TheModule, llvm::Type *flt, llvm::legacy::FunctionPassManager *TheFPM) {
    llvm::FunctionType *int_to_float = llvm::FunctionType::get(flt, {llvm::Type::getInt32Ty(TheModule->getContext())}, false);
    llvm::Function *floatOfIntFunc = llvm::Function::Create(int_to_float, llvm::Function::InternalLinkage, "float_of_int", TheModule);
    llvm::BasicBlock *floatOfIntBB = llvm::BasicBlock::Create(TheModule->getContext(), "entry", floatOfIntFunc);
    llvm::IRBuilder<> TmpB(TheModule->getContext()); TmpB.SetInsertPoint(floatOfIntBB);
    llvm::Value *newFloat = TmpB.CreateCast(llvm::Instruction::SIToFP, floatOfIntFunc->getArg(0), flt, "newfloat");
    TmpB.CreateRet(newFloat);
    TheFPM->run(*floatOfIntFunc);
    return floatOfIntFunc;
}

std::vector<std::pair<std::string, llvm::Function*>>* AST::genLibGlueLogic() {
    auto pairs = new std::vector<std::pair<std::string, llvm::Function*>>();
    llvm::FunctionType 
        *int_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i32}, false),
        *bool_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i1}, false),
        *char_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false),
        *float_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {flt}, false),
        *arrchar_to_unit = llvm::FunctionType::get(unitType, {arrCharType}, false),
        *string_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8->getPointerTo()}, false),
        *void_to_int = llvm::FunctionType::get(i32, {}, false),
        *void_to_bool = llvm::FunctionType::get(i1, {}, false),
        *void_to_char = llvm::FunctionType::get(i8, {}, false),
        *void_to_float = llvm::FunctionType::get(flt, {}, false),
        *string_to_int = llvm::FunctionType::get(i32, {i8->getPointerTo()}, false),
        *int_string_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i32, i8->getPointerTo()}, false),
        *string_string_to_void = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8->getPointerTo(), i8->getPointerTo()}, false),
        *string_string_to_int = llvm::FunctionType::get(i32, {i8->getPointerTo(), i8->getPointerTo()}, false);
    llvm::Function 
        *ReadInteger = llvm::Function::Create(void_to_int, llvm::Function::ExternalLinkage, "readInteger", TheModule),
        *ReadBool = llvm::Function::Create(void_to_bool, llvm::Function::ExternalLinkage, "readBoolean", TheModule),
        *ReadChar = llvm::Function::Create(void_to_char, llvm::Function::ExternalLinkage, "readChar", TheModule),
        *ReadFloat = llvm::Function::Create(void_to_float, llvm::Function::ExternalLinkage, "readReal", TheModule),
        *ReadString = llvm::Function::Create(int_string_to_void, llvm::Function::ExternalLinkage, "readString", TheModule),
        *WriteInteger = llvm::Function::Create(int_to_void, llvm::Function::ExternalLinkage, "writeInteger", TheModule),
        *WriteBool = llvm::Function::Create(bool_to_void, llvm::Function::ExternalLinkage, "writeBoolean", TheModule),
        *WriteChar = llvm::Function::Create(char_to_void, llvm::Function::ExternalLinkage, "writeChar", TheModule),
        *WriteFloat = llvm::Function::Create(float_to_void, llvm::Function::ExternalLinkage, "writeReal", TheModule),
        *WriteString = llvm::Function::Create(string_to_void, llvm::Function::ExternalLinkage, "writeString", TheModule),
        *StrLen = llvm::Function::Create(string_to_int, llvm::Function::ExternalLinkage, "strlen", TheModule),
        *StrCpy = llvm::Function::Create(string_string_to_void, llvm::Function::ExternalLinkage, "strcpy", TheModule),
        *StrCmp = llvm::Function::Create(string_string_to_int, llvm::Function::ExternalLinkage, "strcmp", TheModule),
        *StrCat = llvm::Function::Create(string_string_to_void, llvm::Function::ExternalLinkage, "strcat", TheModule);
    std::vector<llvm::Function *> IOlib = {ReadInteger, ReadBool, ReadChar, ReadFloat,
                                           WriteInteger, WriteBool, WriteChar, WriteFloat, WriteString,
                                           StrCpy, StrCat};
    std::vector<llvm::Function *> IOlibAdapted;
    std::transform(IOlib.begin(), IOlib.end(),
        std::back_inserter(IOlibAdapted),
        AST::createFuncAdapterFromVoidToUnit
    );
    IOlibAdapted[8] = createFuncAdapterFromStringToCharArr(IOlibAdapted[8]); // WriteString
    IOlibAdapted[9] = createFuncAdapterFromStringToCharArr(IOlibAdapted[9]); // StrCpy
    IOlibAdapted[10] = createFuncAdapterFromStringToCharArr(IOlibAdapted[10]); //StrCat

    IOlibAdapted.push_back(adaptReadString(ReadString, TheModule, arrchar_to_unit, unitVal(),
                                           c32(0), c32(1), c32(2), TheFPM));
    IOlibAdapted.push_back(createFuncAdapterFromStringToCharArr(StrLen));
    IOlibAdapted.push_back(createFuncAdapterFromStringToCharArr(StrCmp));

    IOlibAdapted[0]->setName("read_int"); IOlibAdapted[1]->setName("read_bool"); 
    IOlibAdapted[2]->setName("read_char"); IOlibAdapted[3]->setName("read_float");
    IOlibAdapted[4]->setName("print_int"); IOlibAdapted[5]->setName("print_bool");
    IOlibAdapted[6]->setName("print_char"); IOlibAdapted[7]->setName("print_float");
    IOlibAdapted[8]->setName("print_string"); IOlibAdapted[11]->setName("read_string"); 
    for (unsigned int i = 0; i < IOlibAdapted.size(); i++) {
        switch (i) {
            case 9: pairs->push_back({"strcpy", IOlibAdapted[i]}); break;
            case 10: pairs->push_back({"strcat", IOlibAdapted[i]}); break;
            case 12: pairs->push_back({"strlen", IOlibAdapted[i]}); break;
            case 13: pairs->push_back({"strcmp", IOlibAdapted[i]}); break;
            default: pairs->push_back({IOlibAdapted[i]->getName(), IOlibAdapted[i]}); 
        }
    }
    llvm::FunctionType 
        *int_to_int = llvm::FunctionType::get(i32, {i32}, false),
        *float_to_float = llvm::FunctionType::get(flt, {flt}, false),
        *float_to_int = llvm::FunctionType::get(i32, {flt}, false),
        *char_to_int = llvm::FunctionType::get(i32, {i8}, false),
        *int_to_char = llvm::FunctionType::get(i8, {i32}, false);
    llvm::Function
        *Abs = llvm::Function::Create(int_to_int, llvm::Function::ExternalLinkage, "abs", TheModule),
        *FAbs = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "fabs", TheModule),
        *Sqrt = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "sqrt", TheModule),
        *Sin = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "sin", TheModule),
        *Cos = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "cos", TheModule),
        *Tan = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "tan", TheModule),
        *Atan = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "atan", TheModule),
        *Exp = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "exp", TheModule),
        *Ln = llvm::Function::Create(float_to_float, llvm::Function::ExternalLinkage, "ln", TheModule),
        *Pi = llvm::Function::Create(void_to_float, llvm::Function::ExternalLinkage, "pi", TheModule),
        *Chr = llvm::Function::Create(int_to_char, llvm::Function::ExternalLinkage, "chr", TheModule),
        *Ord = llvm::Function::Create(char_to_int, llvm::Function::ExternalLinkage, "ord", TheModule),
        *Exit = llvm::Function::Create(int_to_void, llvm::Function::ExternalLinkage, "_exit", TheModule),
        *Round = llvm::Function::Create(float_to_int, llvm::Function::ExternalLinkage, "round", TheModule),
        *Trunc = llvm::Function::Create(float_to_int, llvm::Function::ExternalLinkage, "trunc", TheModule);
    std::vector<llvm::Function *> UtilLib = {Abs, FAbs, Sqrt, Sin, Cos, Tan, Atan, Exp, Ln, 
                                             Exit, Round};
    for (auto &func: UtilLib) {
        pairs->push_back({func->getName(), func});
    }
    pairs->push_back({"pi", createFuncAdapterFromVoidToUnit(Pi)});
    pairs->push_back({"int_of_float", Trunc});
    pairs->push_back({"int_of_char", Ord});
    pairs->push_back({"char_of_int", Chr});

    pairs->push_back({"incr", createIncrLibFunc(TheModule, unitType, c32(1), unitVal(), TheFPM)});
    pairs->push_back({"decr", createDecrLibFunc(TheModule, unitType, c32(1), unitVal(), TheFPM)});
    pairs->push_back({"float_of_int", createFloatOfIntLibFunc(TheModule, flt, TheFPM)});

    llvm::FunctionType *powType = 
        llvm::FunctionType::get(flt, {flt, flt}, false);
    llvm::Function *pow =
        llvm::Function::Create(powType, llvm::Function::ExternalLinkage, "pow.custom", TheModule);
    llvm::BasicBlock *powBB = llvm::BasicBlock::Create(TheContext, "entry", pow);
    llvm::BasicBlock *signApplierBB = llvm::BasicBlock::Create(TheContext, "signapply", pow);
    llvm::BasicBlock *collectorBB = llvm::BasicBlock::Create(TheContext, "collector", pow);
    llvm::IRBuilder<> TmpB(TheContext); TmpB.SetInsertPoint(powBB);

    llvm::Value *isNegative = TmpB.CreateFCmpOLT(pow->getArg(0), f80(0.0), "pow.xisnegative");
    llvm::Value *absX = TmpB.CreateCall(FAbs, {pow->getArg(0)}, "pow.absx");
    llvm::Value *logarithm = TmpB.CreateCall(Ln, {absX}, "pow.lnabsx");
    llvm::Value *mult = TmpB.CreateFMul(pow->getArg(1), logarithm, "pow.ylnx");
    llvm::Value *powRes = TmpB.CreateCall(Exp, {mult}, "pow.res");
    TmpB.CreateCondBr(isNegative, signApplierBB, collectorBB);

    TmpB.SetInsertPoint(signApplierBB);
    llvm::Value *negRes = TmpB.CreateFNeg(powRes);
    TmpB.CreateBr(collectorBB);

    TmpB.SetInsertPoint(collectorBB);
    llvm::PHINode *fullRes = TmpB.CreatePHI(flt, 2, "pow.signrestore");
    fullRes->addIncoming(powRes, powBB);
    fullRes->addIncoming(negRes, signApplierBB);
    TmpB.CreateRet(fullRes);
    
    TheFPM->run(*pow);
    return pairs;
}

llvm::Type* TypeGraph::getLLVMType(llvm::Module *TheModule){
    return nullptr;
}
llvm::Type* UnknownTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    TypeGraph *correctTypeGraph = inf.deepReplace(this);
    return correctTypeGraph->getLLVMType(TheModule);
}
llvm::Type* UnitTypeGraph::getLLVMType(llvm::Module *TheModule) 
{
    if (llvm::Type* unitType = TheModule->getTypeByName("unit")) {
        return unitType;
    } else {
        llvm::StructType* newUnitType = llvm::StructType::create(TheModule->getContext());
        newUnitType->setName("unit");
        newUnitType->setBody({llvm::Type::getInt1Ty(TheModule->getContext())});
        return newUnitType;
    }
}
llvm::IntegerType* IntTypeGraph::getLLVMType(llvm::Module *TheModule)
{ 
    return llvm::Type::getInt32Ty(TheModule->getContext());
}
llvm::IntegerType* CharTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    return llvm::Type::getInt8Ty(TheModule->getContext());
}
llvm::IntegerType* BoolTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    return llvm::Type::getInt1Ty(TheModule->getContext());
}
llvm::Type* FloatTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    return llvm::Type::getX86_FP80Ty(TheModule->getContext());
}
llvm::PointerType* ArrayTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    // Get element type
    TypeGraph *elementTypeGraph = inf.deepReplace(this)->getContainedType()->getContainedType();
    llvm::Type *elementLLVMType = elementTypeGraph->getLLVMType(TheModule);

    std::string arrayTypeName = std::string("Array") + '.' + std::to_string(dimensions) + '.' + elementTypeGraph->makeTypeString2(); 

    llvm::StructType *LLVMArrayType;
    if((LLVMArrayType = TheModule->getTypeByName(arrayTypeName)))
    {
        return LLVMArrayType->getPointerTo();
    }

    std::vector<llvm::Type *> members;

    llvm::PointerType *arrayPointer = elementLLVMType->getPointerTo();
    members.push_back(arrayPointer);
    
    llvm::IntegerType *LLVMInteger = llvm::Type::getInt32Ty(TheModule->getContext());
    members.insert(members.end(), dimensions + 1, LLVMInteger);
    
    LLVMArrayType = llvm::StructType::create(TheModule->getContext(), arrayTypeName);
    LLVMArrayType->setBody(members);

    return LLVMArrayType->getPointerTo();
}
llvm::PointerType* RefTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    llvm::Type *containedLLVMType = this->Type->getLLVMType(TheModule);
    return containedLLVMType->getPointerTo();
}
std::vector<llvm::Type *> FunctionTypeGraph::getLLVMParamTypes(llvm::Module *TheModule)
{
    std::vector<llvm::Type *> LLVMParamTypes = {};
    for(auto p: *paramTypes)
    {
        LLVMParamTypes.push_back(p->getLLVMType(TheModule));
    }
    return LLVMParamTypes;
}
llvm::Type *FunctionTypeGraph::getLLVMResultType(llvm::Module *TheModule)
{
    return resultType->getLLVMType(TheModule);
}
llvm::PointerType* FunctionTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    llvm::Type *LLVMResultType = getLLVMResultType(TheModule);

    auto LLVMParamTypes = getLLVMParamTypes(TheModule);
    llvm::FunctionType *tempType = llvm::FunctionType::get(LLVMResultType, LLVMParamTypes, false);
    return tempType->getPointerTo();
}
llvm::StructType* ConstructorTypeGraph::getLLVMType(llvm::Module *TheModule)
{
    std::vector<llvm::Type *> LLVMTypeList = {};

    for(auto f: *fields)
    {
        LLVMTypeList.push_back(f->getLLVMType(TheModule));
    }

    return llvm::StructType::get(TheModule->getContext(), LLVMTypeList);
}

llvm::PointerType* CustomTypeGraph::getLLVMType(llvm::Module *TheModule)
{   
    llvm::StructType *LLVMCustomType;
    if ((LLVMCustomType = TheModule->getTypeByName(name))) {
        return LLVMCustomType->getPointerTo();
    }

    LLVMCustomType = llvm::StructType::create(TheModule->getContext(), name);
    
    const llvm::DataLayout &TheDataLayout = TheModule->getDataLayout();
    llvm::StructType *LLVMLargestStructType = nullptr, *LLVMTempType;
    bool first = true;
    int prevSize, currSize;
    for(auto c: *constructors)
    {
        LLVMTempType = c->getLLVMType(TheModule);

        if(first) 
        {
            LLVMLargestStructType = LLVMTempType;
            first = false;
            continue;
        }

        prevSize = TheDataLayout.getTypeAllocSize(LLVMLargestStructType);
        currSize = TheDataLayout.getTypeAllocSize(LLVMTempType);
        if(prevSize < currSize) LLVMLargestStructType = LLVMTempType;
    }

    llvm::IntegerType *LLVMStructEnum = llvm::Type::getInt32Ty(TheModule->getContext());
    std::vector<llvm::Type *> LLVMStructTypes = { LLVMStructEnum, LLVMLargestStructType };
    LLVMCustomType->setBody(LLVMStructTypes);
    
    return LLVMCustomType->getPointerTo();
}

// defined here instead of genIR.cpp for linking order reasons
llvm::Value *AST::equalityHelper(llvm::Value *lhsVal,
                                 llvm::Value *rhsVal,
                                 TypeGraph *type,
                                 bool structural,
                                 llvm::IRBuilder<> TmpB)
{
    if (type->isUnit()) {
        return c1(true);
    }
    if (type->isCustom() && structural)
    {   
        if (CustomTypeGraph *tmpCstType = dynamic_cast<CustomTypeGraph*>(type)) {
            llvm::Function *cstTypeEqFunc = tmpCstType->getStructEqFunc(TheModule, TheFPM);
            return TmpB.CreateCall(cstTypeEqFunc, {lhsVal, rhsVal}, "strcteq.equals");
        }
        else { // internal error
            std::cerr << "Internal error: equalityHelper impossible else entered\n";
            exit(1);
        }
    }
    if ((type->isCustom() && !structural) || type->isRef()) {
        llvm::Value 
            *lhsPointerInt = TmpB.CreatePtrToInt(lhsVal, machinePtrType, "ptr.cmplhstmp"),
            *rhsPointerInt = TmpB.CreatePtrToInt(rhsVal, machinePtrType, "ptr.cmprhstmp");
        return TmpB.CreateICmpEQ(lhsPointerInt, rhsPointerInt, "ptr.cmpnateqtmp");         
    }
    if (type->isInt() || type->isBool() || type->isChar()) {
        return TmpB.CreateICmpEQ(lhsVal, rhsVal, "int.cmpeqtmp");
    }
    if (type->isFloat()) {
        return TmpB.CreateFCmpOEQ(lhsVal, rhsVal, "float.cmpeqtmp");
    }
    std::cerr << "Structural equality attempted of custom types containing array or function field\n";
    exit(1);
}


llvm::Function *CustomTypeGraph::getStructEqFunc(llvm::Module *TheModule, 
                                                 llvm::legacy::FunctionPassManager *TheFPM) {

    // if it has been already declared and saved, then just return it
    if (structEqFunc)
        return structEqFunc;

    // else declare and define it
    auto &TheContext = TheModule->getContext();
    auto c32 = [&](int n) { 
        return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, false));
    };
    auto c1 = [&](bool b) {
        return llvm::ConstantInt::get(TheContext, llvm::APInt(1, b, false));
    };
    auto *structLLVMType = getLLVMType(TheModule);
    auto *eqFuncType = llvm::FunctionType::get(
        llvm::Type::getInt1Ty(TheContext),
        {structLLVMType, structLLVMType},
        false
    );
    structEqFunc = llvm::Function::Create(
        eqFuncType,
        llvm::Function::InternalLinkage,
        name + ".strcteq",
        TheModule
    );
    llvm::Value *lhsVal = structEqFunc->getArg(0),
                *rhsVal = structEqFunc->getArg(1);

    auto *entryBB = llvm::BasicBlock::Create(TheContext, "entry", structEqFunc),
         *exitBB = llvm::BasicBlock::Create(TheContext, "exit", structEqFunc),
         *switchBB = llvm::BasicBlock::Create(TheContext, "switch.init", structEqFunc),
         *errorBB = llvm::BasicBlock::Create(TheContext, "error", structEqFunc);
    llvm::IRBuilder<> TmpB(TheContext);
    TmpB.SetInsertPoint(exitBB);
    std::size_t incomingCount = 0;
    for (auto &constr: *constructors) {
        std::size_t fieldCount = constr->getFieldCount();
        // if it has no fields, still add one incoming
        incomingCount += fieldCount ? fieldCount : 1;
    }
    auto *resPhi = TmpB.CreatePHI(
        llvm::Type::getInt1Ty(TheContext),
        1 + incomingCount, //TODO(ORF): Make sure this is correct
        name + ".strcteq.res"
    );

    llvm::Value *lhsFieldLoc, *lhsField, *rhsFieldLoc, *rhsField, *compRes;
    TmpB.SetInsertPoint(entryBB);
    lhsFieldLoc = TmpB.CreateGEP(lhsVal, {c32(0), c32(0)}, "strcteq.lhstypeloc");
    lhsField = TmpB.CreateLoad(lhsFieldLoc);
    rhsFieldLoc = TmpB.CreateGEP(rhsVal, {c32(0), c32(0)}, "strcteq.rhstypeloc");
    rhsField = TmpB.CreateLoad(rhsFieldLoc);
    compRes = TmpB.CreateICmpEQ(
        lhsField,
        rhsField,
        "strcteq.sametypetmp"
    );
    // comparison fails if not of the same constructor type
    TmpB.CreateCondBr(compRes, switchBB, exitBB);
    resPhi->addIncoming(compRes, entryBB);

    // switch logic init
    TmpB.SetInsertPoint(switchBB);
    llvm::Value *lhsInnerStructLoc = TmpB.CreateGEP(
                    lhsVal, {c32(0), c32(1)}, "strcteq.lhsconstrloc"),
                *rhsInnerStructLoc = TmpB.CreateGEP(
                    rhsVal, {c32(0), c32(1)}, "strcteq.rhsconstrloc");
    llvm::Value *constrType = lhsField; // save the type of constr it is
    std::vector<llvm::BasicBlock *> switchTypeBBs;
    auto *typeSwitch = 
        TmpB.CreateSwitch(constrType, errorBB, constructors->size());
    llvm::BasicBlock *currentBB;
    for (std::size_t i = 0; i < constructors->size(); i++) {
        // one switch case for each constructor type
        currentBB = 
            llvm::BasicBlock::Create(
                TheContext,
                std::string("case.") + (*constructors)[i]->getName(),
                structEqFunc
            );
        switchTypeBBs.push_back(currentBB);
        typeSwitch->addCase(c32(i), currentBB);
    }
    
    // default / error BB code
    TmpB.SetInsertPoint(errorBB);
    TmpB.CreateCall(TheModule->getFunction("writeString"),
        {TmpB.CreateGlobalStringPtr("Internal error: Invalid constructor enum\n")});
    TmpB.CreateCall(TheModule->getFunction("_exit"), {c32(1)});
    TmpB.CreateBr(errorBB); // to avoid llvm error

    // for every constructor type
    for (std::size_t i = 0; i < constructors->size(); i++) {
        llvm::Value *lhsCastedVal, *rhsCastedVal;
        ConstructorTypeGraph *currConstrGraph = (*constructors)[i];
        llvm::StructType *currConstrType = currConstrGraph->getLLVMType(TheModule);
        currentBB = switchTypeBBs[i];
        TmpB.SetInsertPoint(currentBB);
        
        // no fields
        if (currConstrGraph->getFieldCount() == 0) {
            TmpB.CreateBr(exitBB);
            resPhi->addIncoming(c1(true), currentBB);
            continue;
        }

        lhsCastedVal = TmpB.CreatePointerCast(
            lhsInnerStructLoc, currConstrType->getPointerTo(), "strcteq.lhscast");
        rhsCastedVal = TmpB.CreatePointerCast(
            rhsInnerStructLoc, currConstrType->getPointerTo(), "strcteq.rhscast");
        // for every field of constructor
        for (int j = 0; j < currConstrGraph->getFieldCount(); j++) {
            lhsFieldLoc = TmpB.CreateGEP(
                lhsCastedVal, {c32(0), c32(j)}, "strcteq.lhsfieldloc");
            lhsField = TmpB.CreateLoad(lhsFieldLoc);
            rhsFieldLoc = TmpB.CreateGEP(
                rhsCastedVal, {c32(0), c32(j)}, "strcteq.rhsfieldloc");
            rhsField = TmpB.CreateLoad(rhsFieldLoc);
            compRes = AST::equalityHelper(
                lhsField, rhsField, currConstrGraph->getFieldType(j), true, TmpB);

            // not the final field            
            if (j != currConstrGraph->getFieldCount() - 1) {
                llvm::BasicBlock *nextFieldBB = 
                    llvm::BasicBlock::Create(
                        TheContext, 
                        std::string("case.") + currConstrGraph->getName() + ".nextfield", 
                        structEqFunc);
                TmpB.CreateCondBr(compRes, nextFieldBB, exitBB);
                resPhi->addIncoming(compRes, TmpB.GetInsertBlock());
                TmpB.SetInsertPoint(nextFieldBB);
            } else { // if it's the final field
                TmpB.CreateBr(exitBB);
                resPhi->addIncoming(compRes, TmpB.GetInsertBlock());
            }
        }
    }
    TmpB.SetInsertPoint(exitBB);
    TmpB.CreateRet(resPhi);

    TheFPM->run(*structEqFunc);

    return structEqFunc;
}
