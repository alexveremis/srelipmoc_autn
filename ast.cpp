#include <string>
#include <cstdio>
#include <cstdlib>

#include "lexer.hpp"
#include "ast.hpp"
#include "parser.hpp"


TypeGraph *type_unit = tt.searchingType("unit")->getTypeGraph();
TypeGraph *type_int = tt.searchingType("int")->getTypeGraph();
TypeGraph *type_float = tt.searchingType("float")->getTypeGraph();
TypeGraph *type_bool = tt.searchingType("bool")->getTypeGraph();
TypeGraph *type_char = tt.searchingType("char")->getTypeGraph();
std::vector<Identifier *> AST_identifier_list = {};

void printStringCompiler(std::string s, int width, bool pErr = false)
{
    std::ostream &outStream = pErr ? std::cerr : std::cout;
    outStream << std::left << std::setfill(' ') << std::setw(width) << s ;
}

void Identifier::printIdLine(int lineWidth, int idWidth, int typeWidth)
{
    printStringCompiler(getName(), idWidth);
    printStringCompiler(getTypeString(), typeWidth);
    printStringCompiler(getLine(), lineWidth);
    std::cerr << std::endl;
}

void AST::printError(std::string msg, bool crash)
{
    std::string intro = "Error at the line: " + std::to_string(line_number) + ": ";
    printStringCompiler(intro, intro.size(), true);
    std::cerr << msg << std::endl;
    if (crash) exit(1);
}
void AST::printIdTypeGraphs()
{
    int margin = 2;
    int idWidth = 4, typeWidth = 8, lineWidth = 3;
    std::string name, typeString, line;
    for (auto ident : AST_identifier_list)
    {
        name = ident->getName();
        typeString = ident->getTypeString();
        line = ident->getLine();

        if (idWidth < (int)name.size())
            idWidth = name.size();
        if (typeWidth < (int)typeString.size())
            typeWidth = typeString.size();
        if (lineWidth < (int)line.size())
            lineWidth = line.size();
    }
    idWidth += margin;
    typeWidth += margin;
    lineWidth += margin;
    // Print the header of the table
    printStringCompiler("Name", idWidth);
    printStringCompiler("Type", typeWidth);
    printStringCompiler("Line", lineWidth);
    std::cout << std::endl;
    for (auto ident : AST_identifier_list)
    {
        ident->printIdLine(lineWidth, idWidth, typeWidth);
    }
}
void same_type(Expr *e1, Expr *e2, std::string msg = "Not the same Type")
{
    e1->type_check(e2->TG, msg);
}

void BinOp::sem() {
	lhs->sem();
	rhs->sem();

	TypeGraph *t_lhs = lhs->get_TypeGraph();
	TypeGraph *t_rhs = rhs->get_TypeGraph();

	switch (op)
	{
	case '+':
	case '-':
	case '*':
	case '/':
	case T_mod:
	{
		lhs->type_check(type_int, "INT should be here");
		rhs->type_check(type_int, "INT should be here");

		TG = type_int;
		break;
	}
	case T_plusdot:
	case T_mindot:
	case T_stardot:
	case T_sldot:
	case T_starstar:
	{
		lhs->type_check(type_float, "FLOAT should be here");
		rhs->type_check(type_float, "FLOAT should be here");

		TG = type_float;
		break;
	}
	case T_barbar:
	case T_andand:
	{
		lhs->type_check(type_bool, "BOOL should be here");
		rhs->type_check(type_bool, "BOOL should be here");

		TG = type_bool;
		break;
	}
	case '=':
	case T_lgr:
	case T_eqeq:
	case T_excleq:
	{
		// Check they are not arrays nor functions
		if (t_lhs->isArray() || t_lhs->isFunction() ||
			t_rhs->isArray() || t_rhs->isFunction())
		{
			printError("Anything BUT Arrays or functions should be here");
		}
		same_type(lhs, rhs);
		TG = type_bool;
		break;
	}
	case '<':
	case '>':
	case T_leq:
	case T_greq:
	{
		lhs->checkIntCharFloat();
		rhs->checkIntCharFloat();
		same_type(lhs, rhs);
		TG = type_bool;
		break;
	}
	case T_coleq:
	{
		// The lhs must be a ref of the same type as the rhs
		RefTypeGraph *correct_lhs = new RefTypeGraph(t_rhs);
		lhs->type_check(correct_lhs, "Should be a Reference of " + t_rhs->makeTypeString());
		// The result must be unit
		TG = type_unit;
		break;
	}
	case ';':
	{
		TG = t_rhs;
	}
	default:
		break;
	}
}


void UnOp::sem(){
	expr->sem();
	TypeGraph *t_expr = expr->get_TypeGraph();

	switch (op)
	{
	case '+':
	case '-':
	{
		expr->type_check(type_int, "iNT should be here");

		TG = type_int;
		break;
	}
	case T_mindot:
	case T_plusdot:
	{
		expr->type_check(type_float, "FLOAT should be here");

		TG = type_float;
		break;
	}
	case T_not:
	{
		expr->type_check(type_bool, "BOOL should be here");

		TG = type_bool;
		break;
	}
	case '!':
	{
		TypeGraph *unknown = new UnknownTypeGraph(false, true, false);
		TypeGraph *ref_t = new RefTypeGraph(unknown);
		inf.addConstraint(t_expr, ref_t, line_number, new std::function<void(void)>(
			[=]() {
				printError(
					std::string("Expected a Reference but found ") + inf.deepReplace(t_expr)->makeTypeString2(),
					false
				);
			}
		));

		TG = ref_t->getContainedType();
		break;
	}
	case T_delete:
	{
		TypeGraph *unknown_t = new UnknownTypeGraph(false, true, false);
		TypeGraph *ref_t = new RefTypeGraph(unknown_t);
		inf.addConstraint(t_expr, ref_t, line_number, new std::function<void(void)>(
			[=]() {
				printError(
					std::string("Expected a Reference but found ") + inf.deepReplace(t_expr)->makeTypeString2(),
					false
				);
			}
		));

		TG = type_unit;
		break;
	}
	default:
		break;
	}
}


void Function::addExternal(ScopeLiveEntry *l){
	if(!l->getNode()) return;
	std::string id = l->getId();
	if(external.find(id) == external.end()) 
	{
		external[id] = l;
		l->getNode()->addFunctionThatNeedsSymbol(this);
	}
}

template <class T>
class ScopeLiveTable
{
    std::vector<std::map<std::string, T> *> *table;
    bool nameInScope(std::string name, std::map<std::string, T> *scope)
    {
        return (scope->find(name) != scope->end());
    }

public:
    ScopeLiveTable() : table(new std::vector<std::map<std::string, T> *>())
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
    int getCurrScope2()
    {
        return (table->size() - 1);
    }
    ~ScopeLiveTable() {}
};

ScopeLiveTable<ScopeLiveEntry *> LTable;


void openScopeOnLTable()	{ LTable.openScope();}
void closeScopeOnLTable() 	{ LTable.closeScope();}
int getCurrScope2(){return LTable.getCurrScope2();}
int getScopeOf(std::string name)
	{
		ScopeLiveEntry *l = LTable[name];
		return l->getScope();
	}
ScopeLiveEntry *searchingSymbolOnLTable(std::string name){ return LTable[name];}
void insertParToLTable(Par *p)
	{
		int scope = LTable.getCurrScope2();
		LTable.insert({p->getId(), new ScopeLiveEntryPar(scope, p)});
	}
int isInScopeOf(std::string id, Function *func)
	{
		int idScope = getScopeOf(id);
		int funcScope = (func == nullptr) ? 0 : func->getScope();

		return (funcScope < idScope);
	}
void checkScopeAndAddExternal(std::string name, Function *f)
	{
		if(!isInScopeOf(name, f))
		{
			f->addExternal(searchingSymbolOnLTable(name));        
		}
	}
void insertExternalToFrom(Function *funcDependent, Function *func)
	{
		int funcDependentScope = funcDependent->getScope();
		for(auto const it: func->external)
		{
			int extSymbolScope = it.second->getScope();
			if(funcDependentScope < extSymbolScope)continue;
			funcDependent->addExternal(it.second);
		}
	}	
void makeVisitedOnLTable(std::string func){ LTable[func]->visit();}
void insertDefToLTable(Def *d)
	{
		int scope = LTable.getCurrScope2();
		LTable.insert({d->getId(), new ScopeLiveEntryDef(scope, d)});
	}	
void insertForToLTable(For *f)
	{
		int scope = LTable.getCurrScope2();
		LTable.insert({f->getId(), new ScopeLiveEntryFor(scope, f)});
	}
void insertPatternIdToLTable(PatternId *p)
	{
		int scope = LTable.getCurrScope2();
		LTable.insert({p->getId(), new ScopeLiveEntryPatternId(scope, p)});
	}
void insertLibraryToLTable()
	{
		std::vector<std::string> libraryFunc = 
		{
			"print_int", "print_bool", "print_char", "print_float", "print_string",
			"read_int", "read_bool", "read_char", "read_float", "read_string",  
			"abs", "fabs", "sqrt", "sin", "cos", "tan", "atan", "exp", "ln", "pi",
			"incr", "decr",
			"float_of_int", "int_of_float", "round", "int_of_char", "char_of_int",
			"strlen", "strcmp", "strcpy", "strcat"
		};
		for(auto name: libraryFunc)
		{
			LTable.insert({name, new ScopeLiveEntryDef(0, nullptr)});
		}
	}	
	
void Function::scopeLive(Function *prevFunc){
	scope = getCurrScope2();
	openScopeOnLTable();
	for(auto p: par_list)insertParToLTable(p);
	expr->scopeLive(this);
	if(prevFunc)insertExternalToFrom(prevFunc, this);
	closeScopeOnLTable();
}
void Program::scopeLive(Function *prevFunc){
	insertLibraryToLTable();
	for (auto *d : definition_list) d->scopeLive(nullptr);
}
void Clause::scopeLive(Function *prevFunc){
		openScopeOnLTable();
		pattern->scopeLive(prevFunc);
		expr->scopeLive(prevFunc);
		closeScopeOnLTable();
}
void ArrayAcc::scopeLive(Function *prevFunc){
		for(auto *e: expr_list) e->scopeLive(prevFunc);
		if(!prevFunc) return;
		checkScopeAndAddExternal(id, prevFunc);
	}
void PatternId::scopeLive(Function *prevFunc){ insertPatternIdToLTable(this);}	
void For::scopeLive(Function *prevFunc) {
	openScopeOnLTable();
	insertForToLTable(this);
	start->scopeLive(prevFunc);
	finish->scopeLive(prevFunc);
	body->scopeLive(prevFunc);
	closeScopeOnLTable();
}	
void Dim::scopeLive(Function *prevFunc) {
	if(!prevFunc) return;
	checkScopeAndAddExternal(id, prevFunc);
}	
void ConstantCall::scopeLive(Function *prevFunc)	{ 
	if(!prevFunc) return;    
	checkScopeAndAddExternal(id, prevFunc);
}

void FunctionCall::scopeLive(Function *prevFunc){
		for(auto *e: expr_list)
		{
			e->scopeLive(prevFunc);
		}

		if(!prevFunc) 
		{
			return;
		}
		checkScopeAndAddExternal(id, prevFunc);
	}
void Letdef::scopeLive(Function *prevFunc) {
		if (recursive)
		{
			for(auto *d: def_list)
			{
				if(d->isDef())
				{
					insertDefToLTable(dynamic_cast<Def *>(d));
				} 
			}
			for(auto *d: def_list)
			{
				d->scopeLive(prevFunc);
				makeVisitedOnLTable(d->getId());
			}

		}
		else
		{
			for(auto *d: def_list)
			{
				d->scopeLive(prevFunc);
			}
			for(auto *d: def_list)
			{
				if(d->isDef())
				{
					insertDefToLTable(dynamic_cast<Def *>(d));
				} 

				makeVisitedOnLTable(d->getId());        
			}
		}
	}	          