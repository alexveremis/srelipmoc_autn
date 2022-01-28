#pragma once

#include <iostream>
#include <cstdlib>
#include <vector>
#include <string>
#include <map>
#include "types.hpp"


class SymbolEntry {
public:
    std::string name;
    TypeGraph *typeGraph;
    TypeGraph* getTypeGraph(){ return typeGraph; }
    SymbolEntry(std::string n, TypeGraph *t): name(n), typeGraph(t) {}
	virtual ~SymbolEntry() {}//{delete typeGraph;}
};
using pScope = std::map<std::string, SymbolEntry *>;
class ConstructorEntry;

class FunctionEntry : public SymbolEntry {
public:
    FunctionEntry(std::string n, TypeGraph *t):SymbolEntry(n, t) {} 
    void addParam(TypeGraph *param, bool push_back = true)	{ getTypeGraph()->addParam(param, push_back);}
    ~FunctionEntry() {};
};

class ArrayEntry : public SymbolEntry {
public:
    ArrayEntry(std::string n, TypeGraph *t): SymbolEntry(n, t) {}
    ~ArrayEntry() {};
};
class RefEntry : public SymbolEntry {
public:
    RefEntry(std::string n, TypeGraph *t): SymbolEntry(n, t) {} 
    ~RefEntry() {};
};

class TypeEntry: public SymbolEntry {
public:
    std::vector<ConstructorEntry *> *constructors;
    void addConstructor(ConstructorEntry *constr);
    TypeEntry(std::string n, TypeGraph *t):SymbolEntry(n,t),constructors(new std::vector<ConstructorEntry *>()) {}
    ~TypeEntry();
};

class ConstructorEntry: public SymbolEntry {
public:
    TypeEntry *typeEntry;
    void setTypeEntry(TypeEntry *t) { typeEntry = t; }
    void addType(TypeGraph *field)  { getTypeGraph()->addField(field);}
    ConstructorEntry(std::string n, TypeGraph *t): SymbolEntry(n,t) {}
    ~ConstructorEntry(){}//{delete typeEntry;}
}; 
class SymbolTable {
    std::vector<std::map<std::string, SymbolEntry *> *> *Table;
    SymbolEntry* insert(SymbolEntry *entry, bool overwrite = true);
    void insertLibFunctions();
    void error(std::string msg, bool failed = true){
		std::cout << "SymbolTable: "<< msg << std::endl;
		if (failed) exit(1);
	}
    void log(std::string msg){ error(msg, false); }
    bool debug;
public:
    SymbolTable(bool debug = false, bool openGlobalScope = true){
		this->debug = debug;
		Table = new std::vector<pScope *>();
		if (openGlobalScope) {openScope();}
		insertLibFunctions();
	}
	~SymbolTable() {
		while(closeScope()); // do nothing 
		delete Table; 
	}
    void openScope(){
		if (debug)log("Opening a new scope");
		Table->push_back(new pScope());
	}
    bool closeScope(bool deleteEntries = true){
		if (debug)log("Closing a scope");
		if (Table->size() != 0) {
			pScope *poppedScope = Table->back();
			Table->pop_back();
			if (deleteEntries) {
				for (auto &pair: *poppedScope) {
					delete pair.second;
				}
			}
			delete poppedScope;
			return true;
		}
		return false;
}

	SymbolEntry* insertBasic(std::string name, TypeGraph *t, bool overwrite = true){
		SymbolEntry* basicEntry = new SymbolEntry(name, t);
		return insert(basicEntry, overwrite);
	}
    // SymbolEntry* insertUnknown(std::string name, AST *node, bool overwrite = true);
    FunctionEntry* insertFunction(std::string name, TypeGraph *resT, bool overwrite = true){
		FunctionTypeGraph* funcType = new FunctionTypeGraph(resT);
		FunctionEntry* funcEntry = new FunctionEntry(name, funcType);
		return dynamic_cast<FunctionEntry *>(insert(funcEntry, overwrite));
	}
    ArrayEntry* insertArray(std::string name, TypeGraph *containedT, int dimensions, bool overwrite = true){
		ArrayTypeGraph* arrType = new ArrayTypeGraph(dimensions, containedT);
		ArrayEntry* arrEntry = new ArrayEntry(name, arrType);
		return dynamic_cast<ArrayEntry *>(insert(arrEntry, overwrite));
	}
    RefEntry* insertRef(std::string name, TypeGraph *pointedT, bool overwrite = true){
		RefTypeGraph* refType = new RefTypeGraph(pointedT);
		RefEntry* refEntry = new RefEntry(name,refType);
		return dynamic_cast<RefEntry *>(insert(refEntry, overwrite));
	}
	
    SymbolEntry* searching(std::string name, bool err = true);
    FunctionEntry* searchingFunction(std::string name, bool err = true){
		if (SymbolEntry* possible = searching(name, err)) {
			if (possible->getTypeGraph()->isFunction()) 
				return dynamic_cast<FunctionEntry *>(possible);
		}
		if (debug)
			log("Function " + name + " was not found");
		return nullptr;
	}
    ArrayEntry* searchingArray(std::string name, bool err = true){
		if (SymbolEntry* possible = searching(name, err)) {
			if (possible->getTypeGraph()->isArray()) 
				return dynamic_cast<ArrayEntry *>(possible);
		}
		if (debug)
			log("Array " + name + " was not found");
		return nullptr;
	}
	
    RefEntry* searchingRef(std::string name, bool err = true){
		if (SymbolEntry* possible = searching(name, err)) {
			if (possible->getTypeGraph()->isRef())
				return dynamic_cast<RefEntry *>(possible);
		} else
			log("Ref " + name + " was not found");
		return nullptr;
	}
    void enableLogs(){ debug = true; }
};

class BaseTable {
	std::string kind;
    void error(std::string msg, bool failed = true){//deprecated
		std::cout << kind << ":" <<" "<< msg << std::endl;
		if (failed) exit(1);
	}
    void log(std::string msg){ error(msg, false); }
    bool debug;
public:
    std::map<std::string, SymbolEntry *> *Table;
    BaseTable(std::string kind = "BaseTable", bool debug = false){
		this->debug = debug;
		this->kind = kind;
		Table = new pScope();
}
    SymbolEntry *searching(std::string name, bool err = true);
    SymbolEntry* insert(SymbolEntry *entry, bool overwrite = false);
    void enableLogs(){ debug = true; }
    virtual ~BaseTable(){
		if (debug)log("destructor called...");
		for (auto &constructor: *Table) {
			delete constructor.second;
		}
		delete Table; 
	}
};

class TypeTable : public BaseTable {
public:
    TypeTable(bool debug = false): BaseTable("TypeTable", debug) {
		insert(new TypeEntry("int",  new IntTypeGraph()));
		insert(new TypeEntry("float", new FloatTypeGraph()));
		insert(new TypeEntry("char", new CharTypeGraph()));
		insert(new TypeEntry("unit" , new UnitTypeGraph()));
		insert(new TypeEntry("bool" , new BoolTypeGraph()));
	}
    TypeEntry* insertType(std::string name, bool overwrite = false){
		CustomTypeGraph *customType = new CustomTypeGraph(name);
		TypeEntry *typeEntry = new TypeEntry(name, customType);
		return dynamic_cast<TypeEntry *>(insert(typeEntry, overwrite));
	}

    TypeEntry* searchingType(std::string name, bool err = true)	{ return dynamic_cast<TypeEntry *>(searching(name, err));}
    ~TypeTable(){/*
		for (auto &pair: *Table) {
			delete pair.second;
		}
		delete Table; */
	}
};

class ConstructorTable : public BaseTable {
public:
    ConstructorTable(bool debug = false):BaseTable("ConstructorTable", debug) {}
    ConstructorEntry* insertConstructor(std::string name, bool overwrite = false){
		ConstructorTypeGraph *constrType = new ConstructorTypeGraph(name);
		ConstructorEntry *constructorEntry = new ConstructorEntry(name, constrType);
		return dynamic_cast<ConstructorEntry *>(insert(constructorEntry, overwrite));
	}
    ConstructorEntry* searchingConstructor(std::string name, bool err = true)	{ return dynamic_cast<ConstructorEntry *>(searching(name, err));}
    ~ConstructorTable(){}
};

//to be sent to symbols.cpp
extern TypeTable tt; 
extern SymbolTable st;
extern ConstructorTable ct;
