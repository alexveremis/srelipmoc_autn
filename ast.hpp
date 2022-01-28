#pragma once

#include <iostream>
#include <iomanip>
#include <map>
#include <vector>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "lexer.hpp"
#include "symbols.hpp"
#include "types.hpp"
#include "type_inference.hpp"


extern const std::string type_string[];
enum class type
{
    TYPE_unit,
    TYPE_int,
    TYPE_float,
    TYPE_bool,
    TYPE_char
};
enum class category
{
    CATEGORY_basic,
    CATEGORY_function,
    CATEGORY_array,
    CATEGORY_ref,
    CATEGORY_custom,
    CATEGORY_unknown
};
void yyerror(const char *msg);

extern TypeGraph *type_unit;
extern TypeGraph *type_int;
extern TypeGraph *type_float;
extern TypeGraph *type_bool;
extern TypeGraph *type_char;


class Identifier
{
protected:
    std::string id;
    TypeGraph *TG;
    int line;

public:
    Identifier(std::string id, int line): id(id), line(line) { TG = st.searching(id)->getTypeGraph();}
	~Identifier(){
		delete TG;
	}
    std::string getName() { return id;}
    std::string getTypeString(){
    TypeGraph *correctTypeGraph = inf.deepReplace(TG);
    return correctTypeGraph->makeTypeString2();
}
    std::string getLine()	{ return std::to_string(line);}
    void printIdLine(int lineWidth, int idWidth, int typeWidth);
};

extern std::vector<Identifier *> AST_identifier_list;


class Function;
class ScopeLiveEntry;
class PatternId;
class Def;
class Par;
class For;

class AST
{
protected:
    int line_number;

    std::vector<Function *> listOfFunctionsThatNeedSymbol = {};

    static llvm::LLVMContext TheContext;
    static llvm::IRBuilder<> Builder;
    static llvm::Module *TheModule;
    static llvm::legacy::FunctionPassManager *TheFPM;

    static llvm::TargetMachine *TargetMachine;

    static llvm::Type *i1;
    static llvm::Type *i8;
    static llvm::Type *i32;
    static llvm::Type *flt;
    static llvm::Type *unitType;
    static llvm::Type *machinePtrType;
    static llvm::Type *arrCharType;

    static llvm::Function *TheMalloc;
    static llvm::Function *TheUncollectableMalloc;

    static llvm::ConstantInt *c1(bool b);
    static llvm::ConstantInt *c8(char c);
    static llvm::ConstantInt *c32(int n);
    static llvm::ConstantInt *c64(long int n);
    static llvm::Constant *f80(long double d);
    static llvm::Constant *unitVal();
    static llvm::Function *createFuncAdapterFromUnitToVoid(llvm::Function *unitFunc);
    static llvm::Function *createFuncAdapterFromCharArrToString(llvm::Function *charArrFunc);
    static llvm::Function *createFuncAdapterFromVoidToUnit(llvm::Function *voidFunc);
    static llvm::Function *createFuncAdapterFromStringToCharArr(llvm::Function *stringFunc);

    llvm::Value *globalLiveValue = nullptr;
public:
    AST()	{ line_number = line_nr;}
    ~AST(){
		delete TheModule;
		delete TheFPM;
		delete TheMalloc;
		delete TheUncollectableMalloc;

	}
    virtual void printOn(std::ostream &out) const = 0;
    virtual void sem(){}
    llvm::Value *getGlobalLiveValue();
    llvm::Value *updateGlobalValue(llvm::Value *newVal);
    virtual void scopeLive(Function *prevFunc){return;}
    void addFunctionThatNeedsSymbol(Function *f){listOfFunctionsThatNeedSymbol.push_back(f);}
    static llvm::Value *equalityHelper(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                       TypeGraph *type, bool structural, llvm::IRBuilder<> TmpB);
    virtual llvm::Value *compile();
    void start_compilation(const char *programName, bool optimize = false);
    std::vector<std::pair<std::string, llvm::Function *>> *genLibGlueLogic();
    void printLLVMIR();
	void emitLLVMIC(const char *filename);
    void emitObjectCode(const char *filename);
    void emitAssemblyCode();
	void emitAssemblyCode(const char *filename);
    void checkTypeGraphs(TypeGraph *t1, TypeGraph *t2, std::function<void(void)> *errCallback){
		inf.addConstraint(t1, t2, line_number, errCallback);
	}
    void printError(std::string msg, bool crash = true);
    virtual void insertToTable(){}
    void insertBasicToSymbolTable(std::string id, TypeGraph *t){ st.insertBasic(id, t);}
    void insertRefToSymbolTable(std::string id, TypeGraph *t){ st.insertRef(id, t);}
    void insertArrayToSymbolTable(std::string id, TypeGraph *contained_type, int d) { st.insertArray(id, contained_type, d);}
    FunctionEntry *insertFunctionToSymbolTable(std::string id, TypeGraph *t) { return st.insertFunction(id, t);}
    void insertTypeToTypeTable(std::string id){
		if (!tt.insertType(id))
		{
			printError("Type: " + id + " has been declared before");
		}
	}
    ConstructorEntry *insertConstructorToConstructorTable(std::string Id){
		ConstructorEntry *c = ct.insertConstructor(Id);
		if (!c)
		{
			std::string type = searchingConstructorFromContstructorTable(Id)->getTypeGraph()->getCustomType()->makeTypeString2();
			printError("Constructor: " + Id + " belongs to the Type " + type);
		}

		return c;
}
    SymbolEntry *searchingBasicFromSymbolTable(std::string id)	{
		SymbolEntry *s = st.searching(id, false);
		if (!s)
		{
			printError("Identifier: " + id + " was not found");
		}

		return s;
}
    ArrayEntry *searchingArrayFromSymbolTable(std::string id){
		ArrayEntry *s = st.searchingArray(id, false);
		if (!s)
		{
			printError("Array: " + id + " was not found");
		}

		return s;
	}
    TypeEntry *searchingTypeFromTypeTable(std::string id){
    TypeEntry *t = tt.searchingType(id, false);
    if (!t)
    {
        printError("Type: " + id + " was not found");
    }

    return t;
}
    ConstructorEntry *searchingConstructorFromContstructorTable(std::string Id){
		ConstructorEntry *c = ct.searchingConstructor(Id, false);
		if (!c)
		{
			printError("Constructor: " + Id + " was not found");
		}

		return c;
}

    void printIdTypeGraphs();
    void addToIdList(std::string id)	{AST_identifier_list.push_back(new Identifier(id, line_number));}
};

std::ostream &operator<<(std::ostream &out, const AST &t);
 

class Type : public AST
{
protected:
    category c;              
    TypeGraph *TG = nullptr; 

public:
    Type(category c): c(c) {}
	virtual ~Type(){
		delete TG;
	}
    category get_category() {return c;}
    bool compare_category(category _c){return c==_c;}
    virtual bool compare_basic_type(type t) {return false;}
    virtual TypeGraph *get_TypeGraph() = 0;
    virtual std::string getTypeStr() const = 0;
    virtual void printOn(std::ostream &out) const override;
    friend bool compare_categories(Type *T1, Type *T2){ return (T1->c == T2->c);}
};
class UnknownType : public Type
{
public:
    UnknownType():Type(category::CATEGORY_unknown){
		TG = new UnknownTypeGraph(true, true, false);
}
    virtual TypeGraph *get_TypeGraph() override 	{ return TG;}
    virtual std::string getTypeStr() const override;
};
class BasicType : public Type
{
protected:
    type t;

public:
    BasicType(type t): Type(category::CATEGORY_basic), t(t){}
    virtual bool compare_basic_type(type _t) override{return t == _t;}
    virtual TypeGraph *get_TypeGraph() override{
		if (!TG)
			TG = searchingTypeFromTypeTable(type_string[(int)t])->getTypeGraph();

		return TG;
}
    virtual std::string getTypeStr() const override;
};
class FunctionType : public Type
{
private:
    Type *lhtype, *rhtype;

public:
    FunctionType(Type *lhtype = new UnknownType, Type *rhtype = new UnknownType):Type(category::CATEGORY_function), lhtype(lhtype), rhtype(rhtype){}
	~FunctionType(){
		delete lhtype;
		delete rhtype;
	}
    virtual TypeGraph *get_TypeGraph() override{
		if (!TG){
        TypeGraph *l = lhtype->get_TypeGraph();
        TypeGraph *r = rhtype->get_TypeGraph();
        if (r->isFunction())
        {
            TG = r;
        }
        else
        {
            TG = new FunctionTypeGraph(r);
        }
        TG->addParam(l, false);
		}
    return TG;
	}
    virtual std::string getTypeStr() const override;
};
class ArrayType : public Type
{
private:
    int dimensions;
    Type *elem_type;

public:
    ArrayType(int dimensions = 1, Type *elem_type = new UnknownType): Type(category::CATEGORY_array), dimensions(dimensions), elem_type(elem_type){}
	~ArrayType(){delete elem_type;}
    virtual TypeGraph *get_TypeGraph() override{
		if (!TG) TG = new ArrayTypeGraph(dimensions, new RefTypeGraph(elem_type->get_TypeGraph()));
		return TG;
	}
    virtual std::string getTypeStr() const override;
};
class RefType : public Type
{
private:
    Type *ref_type;

public:
    RefType(Type *ref_type = new UnknownType()):Type(category::CATEGORY_ref), ref_type(ref_type){}
	~RefType()	{delete ref_type;}
    virtual TypeGraph *get_TypeGraph() override{
		if (!TG)TG = new RefTypeGraph(ref_type->get_TypeGraph());
		return TG;
	}
    virtual std::string getTypeStr() const override;
};
class CustomType : public Type
{
private:
    std::string id;

public:
    CustomType(std::string *id):Type(category::CATEGORY_custom), id(*id){}
    virtual TypeGraph *get_TypeGraph() override{
		if (!TG) TG = searchingTypeFromTypeTable(id)->getTypeGraph();
		return TG;
	}
    virtual std::string getTypeStr() const override;
};

class Expr : public AST
{
protected:
    Type *T;
    TypeGraph *TG;

public:
	virtual ~Expr(){
		delete T;
		delete TG;
	}
	TypeGraph *get_TypeGraph(){return TG;}
    void type_check(TypeGraph *t, std::string msg = "Wrong Type"){
		checkTypeGraphs(TG, t, new std::function<void(void)>(
        [=]() {
            printError(
                msg + ", " + inf.deepReplace(TG)->makeTypeString2() + " was given.",
                false
            );
        }
    ));
}
    void checkIntCharFloat(std::string msg = "Must be Int, Char or Float"){
		if (!TG->isUnknown())
		{
			if (!TG->equals(type_int) && !TG->equals(type_char) && !TG->equals(type_float))
			{
				printError(msg);
			}
		}
		else
		{
			TG->setIntCharFloat();
		}
	}
    friend void same_type(Expr *e1, Expr *e2, std::string msg);
};


class Constr : public AST
{
private:
    std::string Id;
    std::vector<Type *> type_list;

public:
    Constr(std::string *Id, std::vector<Type *> *t): Id(*Id), type_list(*t) {}
	virtual ~Constr(){while(!type_list.empty()) delete type_list.back(), type_list.pop_back();}
    void add_Id_to_ct(TypeEntry *te){
		ConstructorEntry *c = insertConstructorToConstructorTable(Id);
		for (Type *t : type_list)
		{
			c->addType(t->get_TypeGraph());
		}
		te->addConstructor(c);
}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};

class Par : public AST
{
private:
    std::string id;
    Type *T;

public:
    Par(std::string *id, Type *t = new UnknownType): id(*id), T(t) {}
	virtual ~Par() {delete T;}
    virtual void insertToTable() override{
		insertBasicToSymbolTable(id, T->get_TypeGraph());
		addToIdList(id);
	}
    TypeGraph *get_TypeGraph()	{ return T->get_TypeGraph();}
    std::string getId()	{return id;}
    virtual void printOn(std::ostream &out) const override;
};


class DefStmt : public AST
{
protected:
    std::string id;

public:
    DefStmt(std::string id): id(id) {}
	virtual ~DefStmt(){}
    virtual bool isDef() const{return false;}
    virtual bool isFunctionDefinition() const{return false;}
    virtual void insertToTable()	{insertTypeToTypeTable(id);}
    virtual llvm::Value *generateTrampoline();
    virtual void generateLLVMPrototype();
    virtual void processEnvBacklog();
    virtual void generateBody();
    std::string getId(){return id;}
    virtual TypeGraph *getTypeGraph(){return nullptr;}
};
class Tdef : public DefStmt
{
private:
    std::vector<Constr *> constr_list;

public:
    Tdef(std::string *id, std::vector<Constr *> *c): DefStmt(*id), constr_list(*c) {}
	~Tdef(){while(!constr_list.empty()) delete constr_list.back(), constr_list.pop_back();}
    virtual void insertToTable() override{}
    virtual void sem() override {
		TypeEntry *t = searchingTypeFromTypeTable(id);
		for (Constr *c : constr_list)c->add_Id_to_ct(t);
    }
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};
class Def : public DefStmt
{
protected:
    Type *T;
    TypeGraph *TG;

public:
    Def(std::string id, Type *t): DefStmt(id), T(t) {}
	~Def(){
		delete T;
		delete TG;
	}
    virtual bool isDef() const override{return true;}
    Type *get_type() 	{ return T;}
    virtual TypeGraph *getTypeGraph() override{return TG;}
};
class Constant : public Def
{
protected:
    Expr *expr;

public:
    Constant(std::string *id, Expr *e, Type *t = new UnknownType): Def(*id, t), expr(e) {}
	~Constant(){delete expr;}
    virtual void sem() override{
		std::string err = "Has to be this type" + T->get_TypeGraph()->makeTypeString();
		expr->sem();
		expr->type_check(T->get_TypeGraph(), err);
}
    virtual void insertToTable() override{
		TG = T->get_TypeGraph();
		insertBasicToSymbolTable(id, TG);
		addToIdList(id);
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override {expr->scopeLive(prevFunc);}
    virtual void printOn(std::ostream &out) const override;
};
class Function : public Constant
{
private:
    std::vector<Par *> par_list;

    int scope = 0;
    std::map<std::string, ScopeLiveEntry *> external = {};

    llvm::Function *funcPrototype;
    llvm::StructType *envStructType;
    std::vector<std::pair<AST *, llvm::Value *>> envBacklog = {};
public:
    Function(std::string *id, std::vector<Par *> *p, Expr *e, Type *t = new UnknownType): 
	Constant(id, e, t), par_list(*p), funcPrototype(nullptr), envStructType(nullptr) {}
    ~Function(){while(!par_list.empty()) delete par_list.back(), par_list.pop_back();}
	virtual void sem() override{
		st.openScope();
		for (Par *p : par_list) p->insertToTable();
		std::string err = "Body of Function must come with a known Type " + T->get_TypeGraph()->makeTypeString();
		expr->sem();
		expr->type_check(T->get_TypeGraph(), err);
		st.closeScope();
	}
    virtual bool isFunctionDefinition() const override{return true;}
    virtual void insertToTable() override{
		FunctionEntry *F = insertFunctionToSymbolTable(id, T->get_TypeGraph());
		for (Par *p : par_list)
		{
			F->addParam(p->get_TypeGraph());
		}
		addToIdList(id);
		TG = F->getTypeGraph();
	}
    llvm::Value *generateTrampoline() override;
    void processEnvBacklog() override;
    void generateLLVMPrototype() override;
    void generateBody() override;
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override; 
    void addExternal(ScopeLiveEntry *l);
    friend void insertExternalToFrom(Function *funcDependent, Function *func);
    std::map<std::string, ScopeLiveEntry *> getExternal()	{ return external;}
    llvm::StructType* getEnvStructType();
    void setScope(int s)	{ scope = s;}
    int getScope(){return scope;}
    virtual void printOn(std::ostream &out) const override;
};
class Mutable : public Def
{
public:
    Mutable(std::string id, Type *T = new UnknownType) : Def(id, T) {}
};
class Array : public Mutable
{
private:
    std::vector<Expr *> expr_list;

public:
    Array(std::string *id, std::vector<Expr *> *e, Type *T = new UnknownType): Mutable(*id, T), expr_list(*e) {}
    ~Array(){while(!expr_list.empty()) delete expr_list.back(), expr_list.pop_back();}
	virtual void sem() override{
		for (Expr *e : expr_list){
			e->sem();
			e->type_check(type_int, "Array dimensions size must be INT");
		}
	}
    int get_dimensions()	{ return expr_list.size();}
    virtual void insertToTable(){
		int d = get_dimensions();
		TypeGraph *t = T->get_TypeGraph();
		RefTypeGraph *contained_type = new RefTypeGraph(t);

		if (!t->isUnknown())
		{
			if (t->isArray())
			{
				printError("Array cannot contain Array");
			}
			insertArrayToSymbolTable(id, contained_type, d);
		}
		else
		{
			TypeGraph *unknown_contained_type = new UnknownTypeGraph(false, true, false);
			insertArrayToSymbolTable(id, unknown_contained_type, d);
			inf.addConstraint(unknown_contained_type, contained_type, line_number);
		}
		addToIdList(id);
		
		TG = new ArrayTypeGraph(d, contained_type);
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		for (auto *e : expr_list)
		{
			e->scopeLive(prevFunc);
		}
	}
    virtual void printOn(std::ostream &out) const override;
};
class Variable : public Mutable
{
public:
    Variable(std::string *id, Type *T = new UnknownType): Mutable(*id, T) {}
    virtual void insertToTable() override{
		TypeGraph *t = T->get_TypeGraph();
		TypeGraph *ref_type = new RefTypeGraph(t);

		if (!t->isUnknown())
		{
			insertRefToSymbolTable(id, t);
		}
		else
		{
			TypeGraph *unknown_ref_type = new UnknownTypeGraph(false, true, false);
			insertBasicToSymbolTable(id, unknown_ref_type);
			inf.addConstraint(unknown_ref_type, ref_type, line_number);
		}

		addToIdList(id);
		
		TG = ref_type;
}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};

class Definition : public AST
{
public:
	Definition(){}
	virtual ~Definition(){}
};
class Letdef : public Definition
{
private:
    bool recursive;
    std::vector<DefStmt *> def_list;

public:
    Letdef(std::vector<DefStmt *> *d, bool rec = false): recursive(rec), def_list(*d) {}
	~Letdef(){while(!def_list.empty()) delete def_list.back(), def_list.pop_back();}
    virtual void sem() override{
		if (recursive)
		{
			for (DefStmt *d : def_list)
			{
				if (!d->isFunctionDefinition())
				{
					printError("It cannot be recursive. Only the function definitions can be recursive");
				}
				d->insertToTable();
			}

			for (DefStmt *d : def_list)
			{
				d->sem();
			}
		}

		else
		{
			for (DefStmt *d : def_list)
			{
				d->sem();
			}

			for (DefStmt *d : def_list)
			{
				d->insertToTable();
			}
		}
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override ;
    virtual void printOn(std::ostream &out) const override;
};
class Typedef : public Definition
{
private:
    std::vector<DefStmt *> tdef_list;

public:
    Typedef(std::vector<DefStmt *> *t): tdef_list(*t) {}
	~Typedef(){while(!tdef_list.empty()) delete tdef_list.back(), tdef_list.pop_back();}
    virtual void sem() override{
		for (DefStmt *td : tdef_list)
		{
			td->insertToTable();
		}

		for (DefStmt *td : tdef_list)
		{
			td->sem();
		}
	}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};


class Program : public AST
{
private:
    std::vector<Definition *> definition_list;

public:
    Program():definition_list() {}
    ~Program(){while(!definition_list.empty()) delete definition_list.back(), definition_list.pop_back();}
    virtual void sem() override{
		for (Definition *d : definition_list)
		{
			d->sem();
		}
}
    void append(Definition *d)	{ definition_list.push_back(d);}
    virtual llvm::Value *compile() override;
	

    virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};


class LetIn : public Expr
{
private:
    Definition *letdef;
    Expr *expr;

public:
    LetIn(Definition *letdef, Expr *expr): letdef(letdef), expr(expr) {}
	~LetIn(){
		delete letdef;
		delete expr;
	}
    virtual void sem() override{
		st.openScope();
		letdef->sem();
		expr->sem();
		st.closeScope();
		TG = expr->get_TypeGraph();
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		letdef->scopeLive(prevFunc);
		expr->scopeLive(prevFunc);
	}
    virtual void printOn(std::ostream &out) const override;
};

class Literal : public Expr
{
public:
	Literal(){}
	virtual ~Literal(){}
    virtual llvm::Value *LLVMCompare(llvm::Value *V);
    static char getChar(std::string c){
		char ans = 0;

		if (c[0] != '\\')
		{
			ans = c[0];
		}
		else if (c[1] == 'x')
		{
			const char hex[2] = {c[2], c[3]};
			ans = strtol(hex, nullptr, 16);
		}
		else
		{
			switch (c[1])
			{
			case 'n':
				ans = '\n';
				break;
			case 't':
				ans = '\t';
				break;
			case 'r':
				ans = '\r';
				break;
			case '0':
				ans = 0;
				break;
			case '\\':
				ans = '\\';
				break;
			case '\'':
				ans = '\'';
				break;
			case '\"':
				ans = '\"';
				break;
			}
		}

		return ans;
	}
};
class String_Const : public Literal
{
private:
    std::string s, originalStr;

public:
    String_Const(std::string *s): s(escapeChars(s->substr(1, s->size() - 2))), originalStr(*s) {}
    virtual void sem() override 	{     TG = new ArrayTypeGraph(1, new RefTypeGraph(type_char));}
    std::string escapeChars(std::string rawStr){
		std::string escapedString;
		for (unsigned int i = 0; i < rawStr.size(); i++)
		{
			if (rawStr[i] != '\\')
			{
				escapedString.push_back(rawStr[i]);
			}
			else
			{
				std::string escapeSeq;
				if (rawStr[i + 1] != 'x')
				{
					escapeSeq = rawStr.substr(i, 2);
					i++;
				}
				else
				{
					escapeSeq = rawStr.substr(i, 4);
					i += 3;
				}
				escapedString.push_back(getChar(escapeSeq));
			}
		}
		return escapedString;
	}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};
class Char_Const : public Literal
{
private:
    std::string c_string;
    char c;

public:
    Char_Const(std::string *c_string): c_string(c_string->substr(1, c_string->size() - 2)){
    c = getChar(this->c_string);
}
    virtual llvm::Value *compile() override;
    virtual llvm::Value *LLVMCompare(llvm::Value *V) override;
    virtual void sem() override	{ TG = type_char;}
    virtual void printOn(std::ostream &out) const override;
};
class Bool_Const : public Literal
{
private:
    bool b;

public:
    Bool_Const(bool b): b(b) {}
    virtual void sem() override	{ TG = type_bool;}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};
class Float_Const : public Literal
{
private:
    double d;

public:
    Float_Const(double d): d(d) {}
    virtual void sem() override 	{ TG = type_float;}
    virtual llvm::Value *compile() override;
    virtual llvm::Value *LLVMCompare(llvm::Value *V) override;
    virtual void printOn(std::ostream &out) const override;
};
class Int_Const : public Literal
{
private:
    int n;

public:
    Int_Const(int n): n(n) {}
	virtual ~Int_Const(){}
    int get_int()	{ return n;}
    virtual void sem() override {TG=type_int;}
    virtual llvm::Value *compile() override;
    virtual llvm::Value *LLVMCompare(llvm::Value *V) override;
    virtual void printOn(std::ostream &out) const override;
};
class Unit : public Literal
{
public:
    Unit(){}
    virtual void sem() override{ TG = type_unit;}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};

class BinOp : public Expr
{
private:
    Expr *lhs, *rhs;
    int op;

public:
    BinOp(Expr *e1, int op, Expr *e2): lhs(e1), rhs(e2), op(op) {}
	~BinOp(){
		delete lhs;
		delete rhs;
	}
	virtual void sem() override;
    llvm::Value *allStructFieldsEqual(llvm::Value *lhsVal,
                                      llvm::Value *rhsVal);
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		lhs->scopeLive(prevFunc);
		rhs->scopeLive(prevFunc);
	}
    virtual void printOn(std::ostream &out) const override;
};
class UnOp : public Expr
{
private:
    Expr *expr;
    int op;

public:
    UnOp(int op, Expr *e): expr(e), op(op) {}
	~UnOp()	{ delete expr;}
    virtual void sem() override;
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override	{ expr->scopeLive(prevFunc);}
    virtual void printOn(std::ostream &out) const override;
};
class New : public Expr
{
    Type *new_type;

public:
    New(Type *t): new_type(t) {}
	~New(){delete new_type;}
    virtual void sem() override {
		TypeGraph *t = new_type->get_TypeGraph();

		if (t->isArray())
		{
			printError("Array Type cannot start with new");
		}

		TG = new RefTypeGraph(t);
}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};

class While : public Expr
{
private:
    Expr *cond, *body;

public:
    While(Expr *e1, Expr *e2): cond(e1), body(e2) {}
	~While(){
		delete cond;
		delete body;
	}
    virtual void sem() override{
		cond->sem();
		body->sem();
		cond->type_check(type_bool, "Condition of While must be BOOL");
		body->type_check(type_unit, "Body of While body must be UNIT");
		TG = type_unit;
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		cond->scopeLive(prevFunc);
		body->scopeLive(prevFunc);
	}
    virtual void printOn(std::ostream &out) const override;
};
class For : public Expr
{
private:
    std::string id;
    std::string step;
    Expr *start, *finish, *body;

public:
    For(std::string *id, Expr *e1, std::string s, Expr *e2, Expr *e3): id(*id), step(s), start(e1), finish(e2), body(e3) {}
	~For(){
		delete start;
		delete body;
		delete finish;
	}
    std::string getId()	{return id;}
    virtual void sem() override{
		st.openScope();
		insertBasicToSymbolTable(id, type_int);
		addToIdList(id);
		start->sem();
		finish->sem();
		body->sem();

		start->type_check(type_int, "Start must be INT");
		finish->type_check(type_int, "Finish must be INT");
		body->type_check(type_unit, "Body of For must be UNIT");
		
		st.closeScope();
		TG = type_unit;
}
    virtual llvm::Value *compile() override;
	virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};
class If : public Expr
{
private:
    Expr *cond, *body, *else_body;

public:
    If(Expr *e1, Expr *e2, Expr *e3 = nullptr): cond(e1), body(e2), else_body(e3) {}
	~If(){
		delete cond;
		delete body;
		delete else_body;
	}
    virtual void sem() override{
		cond->sem();
		cond->type_check(type_bool, "If Condition must be BOOL");

		if (else_body == nullptr)
		{
			body->sem();
			body->type_check(type_unit, "If must return UNIT when there is no else");
		}
		else
		{
			body->sem();
			else_body->sem();
			same_type(body, else_body, "If and Else must return SAME Type");
		}

		TG = body->get_TypeGraph();
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		cond->scopeLive(prevFunc);
		body->scopeLive(prevFunc);
		if (else_body != nullptr)else_body->scopeLive(prevFunc);	
	}
    virtual void printOn(std::ostream &out) const override;
};

class Dim : public Expr
{
private:
    Int_Const *dim;
    std::string id;
public:
    Dim(std::string *id, Int_Const *dim = new Int_Const(1)): dim(dim), id(*id) {}
	~Dim(){delete dim;}
    virtual void sem() override{
		SymbolEntry *arr = searchingBasicFromSymbolTable(id);
		int i = dim->get_int();
		if (i < 1)
		{
			printError("Index out of bounds");
		}
		ArrayTypeGraph *constraintArray =  new ArrayTypeGraph(-1, new UnknownTypeGraph(), i);
		inf.addConstraint(arr->getTypeGraph(), constraintArray, line_number, new std::function<void(void)>(
			[=]() {
				printError(
					std::string("Needs array of at least ") + std::to_string(i) + " dimensions",
					false
				);
			}
		));
		TG = type_int;
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};

class ConstantCall : public Expr
{
protected:
    std::string id;

public:
    ConstantCall(std::string *id): id(*id) {}
	virtual ~ConstantCall() {}
    virtual void sem() override{
		SymbolEntry *s = searchingBasicFromSymbolTable(id);
		TG = s->getTypeGraph();
	}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override	;
    virtual void printOn(std::ostream &out) const override;
};
class FunctionCall : public ConstantCall
{
private:
    std::vector<Expr *> expr_list;
    Function *f;

public:
    FunctionCall(std::string *id, std::vector<Expr *> *expr_list): ConstantCall(id), expr_list(*expr_list) {}
	~FunctionCall(){
		delete f;
		while(!expr_list.empty()) delete expr_list.back(), expr_list.pop_back();
	}
    virtual void sem() override{
		TypeGraph *definitionTypeGraph = searchingBasicFromSymbolTable(id)->getTypeGraph();
		int count;
		if (definitionTypeGraph->isUnknown())
		{
			count = (int)expr_list.size();
			UnknownTypeGraph *resultTypeGraph = new UnknownTypeGraph(true, false, false);
			FunctionTypeGraph *callTypeGraph = new FunctionTypeGraph(resultTypeGraph);
			TypeGraph *argTypeGraph;
			for (int i = 0; i < count; i++)
			{
				expr_list[i]->sem();
				argTypeGraph = expr_list[i]->get_TypeGraph();
				callTypeGraph->addParam(argTypeGraph, true);
			}

			inf.addConstraint(definitionTypeGraph, callTypeGraph, line_number);

			TG = resultTypeGraph;
		}
		else if (definitionTypeGraph->isFunction())
		{
			count = definitionTypeGraph->getParamCount();
			if (count > (int)expr_list.size())
			{
				printError("You can only call the whole of Function");
			}
			if (count < (int)expr_list.size())
			{
				printError("Given more arguments than expected for this function");
			}

			std::string err = "Wrong Type at Parameter ";
			TypeGraph *correct_t;
			for (int i = 0; i < count; i++)
			{
				correct_t = definitionTypeGraph->getParamType(i);

				expr_list[i]->sem();
				expr_list[i]->type_check(correct_t, err + std::to_string(i + 1) + ", " + correct_t->makeTypeString2() + " was expected");
			}

			TG = definitionTypeGraph->getResultType();
		}
		else
		{
			printError(id + " has been declared before but not as a function");
		}
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};
class ConstrCall : public Expr
{
private:
    std::string Id;
    std::vector<Expr *> expr_list;
    ConstructorTypeGraph *constructorTypeGraph = nullptr;

public:
    ConstrCall(std::string *Id, std::vector<Expr *> *expr_list = new std::vector<Expr *>()): Id(*Id), expr_list(*expr_list) {}
	virtual ~ConstrCall(){
		while(!expr_list.empty()) delete expr_list.back(), expr_list.pop_back();
		delete constructorTypeGraph;
	}
    virtual void sem() override{
		ConstructorEntry *c = searchingConstructorFromContstructorTable(Id);
		constructorTypeGraph = dynamic_cast<ConstructorTypeGraph *>(c->getTypeGraph());

		int count = constructorTypeGraph->getFieldCount();
		if (count != (int)expr_list.size())
		{
			printError("Cannot call partial pattern of Constructor");
		}

		std::string err = "Wrong Type at: ";
		TypeGraph *correct_t;
		for (int i = 0; i < count; i++)
		{
			correct_t = constructorTypeGraph->getFieldType(i);

			expr_list[i]->sem();
			expr_list[i]->type_check(correct_t, err + std::to_string(i + 1));
		}

		TG = c->getTypeGraph()->getCustomType();
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override	{ for (auto *e: expr_list) e->scopeLive(prevFunc);}
    virtual void printOn(std::ostream &out) const override;
};
class ArrayAcc : public Expr
{
private:
    std::string id;
    std::vector<Expr *> expr_list;

public:
    ArrayAcc(std::string *id, std::vector<Expr *> *expr_list): id(*id), expr_list(*expr_list) {}
	~ArrayAcc(){while(!expr_list.empty()) delete expr_list.back(), expr_list.pop_back();}
    virtual void sem() override{
		int args_n = (int)expr_list.size();
		SymbolEntry *a = searchingBasicFromSymbolTable(id);
		TypeGraph *t = a->getTypeGraph();

		// If dimensions are correct
		if (!t->isUnknown())
		{
			if (!t->isArray())
			{
				printError("Array Access at " + t->makeTypeString2());
			}
			int count = t->getDimensions();
			if (count != args_n)
			{
				printError("You can only call the whole of Array");
			}

			for (int i = 0; i < count; i++)
			{
				expr_list[i]->sem();
				expr_list[i]->type_check(type_int, "Array cells must contain INT");
			}

			TG = t->getContainedType();
		}
		else
		{
			TypeGraph *elemTypeGraph = new RefTypeGraph(new UnknownTypeGraph(false, true, false));
			ArrayTypeGraph *correct_array = new ArrayTypeGraph(args_n, elemTypeGraph);
			inf.addConstraint(t, correct_array, line_number);

			for (Expr *e : expr_list)
			{
				e->sem();
				e->type_check(type_int, "Array cells must contain INT");
			}

			TG = elemTypeGraph;
		}
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};


class Pattern : public AST
{
protected:
    llvm::Value *toMatchV = nullptr;
    llvm::BasicBlock *NextClauseBB = nullptr;

public:
	Pattern(){}
	virtual ~Pattern(){
		delete NextClauseBB;
	}
    virtual void checkPatternTypeGraph(TypeGraph *t){}
    void set_toMatchV(llvm::Value *v);
    void set_NextClauseBB(llvm::BasicBlock *b);
};
class Pattern_Const : public Pattern
{
protected:
    Literal *literal;

public:
    Pattern_Const(Literal *l): literal(l) {}
	~Pattern_Const(){delete literal;}
    virtual void checkPatternTypeGraph(TypeGraph *t) override{
		literal->sem();
		checkTypeGraphs(t, literal->get_TypeGraph(), new std::function<void(void)>(
        [=]() {
            printError("Literal doesn't match the Type given", false);
        }
    ));
	}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};
class PatternId : public Pattern
{
protected:
    std::string id;
    TypeGraph *TG;

public:
    PatternId(std::string *id): id(*id) {}
	virtual ~PatternId(){delete TG;}
    std::string getId()	 { return id;}
    TypeGraph *getTypeGraph() {return TG;}
    virtual void checkPatternTypeGraph(TypeGraph *t) override{
		insertBasicToSymbolTable(id, t);
		addToIdList(id);
		TG = t;
	}
    virtual llvm::Value *compile() override;
	
    virtual void scopeLive(Function *prevFunc) override ;
    virtual void printOn(std::ostream &out) const override;
};
class PatternConstr : public Pattern
{
protected:
    std::string Id;
    std::vector<Pattern *> pattern_list;
    ConstructorTypeGraph *constrTypeGraph;

public:
    PatternConstr(std::string *Id, std::vector<Pattern *> *p_list = new std::vector<Pattern *>()): Id(*Id), pattern_list(*p_list){
		constrTypeGraph = nullptr;
}
    ~PatternConstr(){
		delete constrTypeGraph;
		while(!pattern_list.empty()) delete pattern_list.back(), pattern_list.pop_back();
	}
	virtual void checkPatternTypeGraph(TypeGraph *t) override{
		ConstructorEntry *c = searchingConstructorFromContstructorTable(Id);
		constrTypeGraph = dynamic_cast<ConstructorTypeGraph *>(c->getTypeGraph());
		checkTypeGraphs(t, constrTypeGraph->getCustomType(), new std::function<void(void)>(
			[=]() {
				printError("Constructor has different Type than the expression trying to match", false);
			}
		));

		int count = constrTypeGraph->getFieldCount();
		if (count != (int)pattern_list.size())
		{
			printError("Cannot insert this pattern of Constructor here.");
		}

		TypeGraph *correct_t;
		for (int i = 0; i < count; i++)
		{
			correct_t = constrTypeGraph->getFieldType(i);

			pattern_list[i]->checkPatternTypeGraph(correct_t);
		}
}
    virtual void scopeLive(Function *prevFunc) override { for (const auto &pattern: pattern_list)pattern->scopeLive(prevFunc);}
    virtual llvm::Value *compile() override;
    virtual void printOn(std::ostream &out) const override;
};

class Clause : public AST
{
private:
    Pattern *pattern;
    Expr *expr;
    TypeGraph *correctPatternTypeGraph = nullptr;

public:
    Clause(Pattern *p, Expr *e): pattern(p), expr(e) {}
	virtual ~Clause(){
		delete pattern;
		delete expr;
		delete correctPatternTypeGraph;
	}
    virtual void sem() override{ st.openScope();
		if (correctPatternTypeGraph == nullptr)  printError("Cannot evaluate the Type of e");
		pattern->checkPatternTypeGraph(correctPatternTypeGraph);
		expr->sem();
		st.closeScope();
	}
    void set_correctPatternTypeGraph(TypeGraph *t){    correctPatternTypeGraph = t;}
    TypeGraph *get_exprTypeGraph()	{ return expr->get_TypeGraph();}
    llvm::Value *tryToMatch(llvm::Value *toMatchV, llvm::BasicBlock *NextClauseBB);
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override;
    virtual void printOn(std::ostream &out) const override;
};
class Match : public Expr
{
private:
    Expr *toMatch;
    std::vector<Clause *> clause_list;

public:
    Match(Expr *e, std::vector<Clause *> *c): toMatch(e), clause_list(*c) {}
	~Match(){
		delete toMatch;
		while(!clause_list.empty()) delete clause_list.back(), clause_list.pop_back();
	}
    virtual void sem() override{
		toMatch->sem();
		TypeGraph *t = toMatch->get_TypeGraph();
		TypeGraph *prev = nullptr, *curr;
		bool first = true;
		for (Clause *c : clause_list)
		{
			c->set_correctPatternTypeGraph(t);
			c->sem();
			if (first)
			{
				prev = c->get_exprTypeGraph();
				first = false;
				continue;
			}
			else
			{
				curr = c->get_exprTypeGraph();
				checkTypeGraphs(prev, curr, new std::function<void(void)>(
					[=]() {
						printError("Matches have different Types", false);
					}
				));
				prev = curr;
			}
		}
		TG = prev;
}
    virtual llvm::Value *compile() override;
    virtual void scopeLive(Function *prevFunc) override{
		toMatch->scopeLive(prevFunc);
		for (auto *c : clause_list)
		{
			c->scopeLive(prevFunc);
		}
	}
    virtual void printOn(std::ostream &out) const override;
};



class ScopeLiveEntry
{
protected:
    int scope;
    bool visited = false;

public:
    ScopeLiveEntry(int scope): scope(scope) {}
    int getScope(){return scope;}
    void visit(){visited = true;}
    bool isVisited(){return visited;}
    virtual std::string getId() = 0;
    virtual TypeGraph *getTypeGraph() = 0;
    virtual AST *getNode() = 0;
};
class ScopeLiveEntryDef
    : public ScopeLiveEntry
{
protected:
    Def *symbolDef;

public:
    ScopeLiveEntryDef(int scope, Def *symbolDef): ScopeLiveEntry(scope), symbolDef(symbolDef) {}
	~ScopeLiveEntryDef(){delete symbolDef;}
    virtual Def *getNode() override{return symbolDef;}
    virtual std::string getId() override {return symbolDef->getId();}
    virtual TypeGraph *getTypeGraph() override { return symbolDef->getTypeGraph();}
};
class ScopeLiveEntryPar
    : public ScopeLiveEntry
{
protected:
    Par *symbolPar;

public:
    ScopeLiveEntryPar(int scope, Par *symbolPar): ScopeLiveEntry(scope), symbolPar(symbolPar) {}
	~ScopeLiveEntryPar(){delete symbolPar;}
    virtual Par *getNode() override{return symbolPar;}
    virtual std::string getId() override {return symbolPar->getId();}
    virtual TypeGraph *getTypeGraph() override {return symbolPar->get_TypeGraph();}
};
class ScopeLiveEntryFor
    : public ScopeLiveEntry
{
protected:
    For *symbolFor;

public: 
    ScopeLiveEntryFor(int scope, For *symbolFor): ScopeLiveEntry(scope), symbolFor(symbolFor) {}
	~ScopeLiveEntryFor(){delete symbolFor;}
    virtual For *getNode() override {return symbolFor;}
    virtual std::string getId() override {return symbolFor->getId();}
    virtual TypeGraph *getTypeGraph() override {return symbolFor->get_TypeGraph();}
};
class ScopeLiveEntryPatternId
    : public ScopeLiveEntry
{
protected:
    PatternId *symbolPatternId;

public:
    ScopeLiveEntryPatternId(int scope, PatternId *symbolPatternId): ScopeLiveEntry(scope), symbolPatternId(symbolPatternId) {}
	~ScopeLiveEntryPatternId(){delete symbolPatternId;}
    virtual PatternId *getNode() override	{ return symbolPatternId;}
    virtual std::string getId() override	{ return symbolPatternId->getId();}
    virtual TypeGraph *getTypeGraph() override { return symbolPatternId->getTypeGraph();}
};
