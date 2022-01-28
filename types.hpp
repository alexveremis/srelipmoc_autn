#ifndef __TYPES_HPP__
#define __TYPES_HPP__

#include <iostream>
#include <vector>
#include <string>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>

enum class graphType { 
	TYPE_unknown, TYPE_unit, TYPE_int, TYPE_float, TYPE_bool, TYPE_char, TYPE_ref, TYPE_array, TYPE_function, TYPE_custom, TYPE_record 
	};

class CustomTypeGraph;
class ConstructorTypeGraph;

class TypeGraph {
    graphType t;
    void wrongCall(std::string functionName){log("Function with name: " + functionName + " was called from wrong subClass. Now aborting");}
public:
    TypeGraph(graphType t):t(t){}
    graphType const & getSubClass(){ return t; }
    virtual std::string makeTypeString()   { return makeTypeString2();}
    void log(std::string msg){std::cout << "TypeGraph of the Type: " << makeTypeString()<< " comes with message:" << std::endl << msg << std::endl;}
    bool isFunction(){ return t == graphType::TYPE_function;     }
    bool isArray()	 { return t == graphType::TYPE_array;     }
    bool isRef()	 { return t == graphType::TYPE_ref;     }
    bool isInt()	 { return t == graphType::TYPE_int;     }
    bool isUnit()	 { return t == graphType::TYPE_unit;     }
    bool isBool()	 { return t == graphType::TYPE_bool;     }
    bool isChar()	 { return t == graphType::TYPE_char;     }
    bool isFloat()	 { return t == graphType::TYPE_float;    }
    bool isCustom()	 { return t == graphType::TYPE_custom;   }
    bool isConstructor(){ return t == graphType::TYPE_record;   }
    bool isUnknown() { return t == graphType::TYPE_unknown;  }
    bool isBasic()	 { return isInt() || isUnit() || isBool() || isChar() || isFloat();}
    bool isDeletable(){ return isFunction() || isArray() || isRef(); }
    bool isUnknownRefOrArray(){return (isRef() || isArray()) && (getContainedType()->isUnknown());}
    virtual bool equals(TypeGraph *o) = 0;
    virtual TypeGraph* getContainedType(){  wrongCall("getContainedType()"); exit(1);}
    virtual int getDimensions(){  wrongCall("getDimensions()"); exit(1);}
    virtual std::vector<TypeGraph *>* getParamTypes(){wrongCall("getParamTypes()"); exit(1);}
    virtual TypeGraph* getResultType()	{ wrongCall("getResultType()"); exit(1);}
    virtual int getParamCount()	{ wrongCall("getParamCount()"); exit(1);}
    virtual void addParam(TypeGraph *param, bool push_back = true){wrongCall("addParam()"); exit(1);}
    virtual TypeGraph* getParamType(unsigned int index){wrongCall("getParamType()"); exit(1);}
    virtual std::vector<TypeGraph *>* getFields(){wrongCall("getFields()"); exit(1);}
    virtual void addField(TypeGraph *field){ wrongCall("addField"); exit(1); }
    virtual void setTypeGraph(CustomTypeGraph *owningType){ wrongCall("setTypeGraph()"); exit(1); }
    virtual CustomTypeGraph* getCustomType(){wrongCall("getCustomType()"); exit(1);}
    virtual int getFieldCount(){wrongCall("getFieldCount()"); exit(1);}
    virtual TypeGraph* getFieldType(unsigned int index){wrongCall("getFieldType()"); exit(1);}
    virtual int getConstructorCount() { wrongCall("getConstructorCount()"); exit(1);}
    virtual void addConstructor(ConstructorTypeGraph *constructor){ wrongCall("addConstructor()"); exit(1);}
    virtual std::vector<ConstructorTypeGraph *>* getConstructors(){ wrongCall("getConstructors()"); exit(1);}
    virtual std::string getTmpName()	{ wrongCall("getTmpName()"); exit(1);}
    virtual unsigned long getId()	{ wrongCall("getId()"); exit(1);}
    virtual bool canBeArray()	{ wrongCall("canBeArray()"); exit(1);}
    virtual bool canBeFunc()	{ wrongCall("canBeFunc()"); exit(1);}
    virtual bool onlyIntCharFloat(){ wrongCall("onlyIntCharFloat()"); exit(1);}
    virtual void setIntCharFloat(){ wrongCall("setIntCharFloat()"); exit(1);}
    virtual void copyConstraintFlags(TypeGraph *o){ wrongCall("copyConstraintFlags()"); exit(1);}
    virtual void changeInner(TypeGraph *replacement, unsigned int index = 0){ wrongCall("changeInner()"); exit(1);}
    virtual std::string makeTypeString2(){
    static const std::string graph_type_string[] = {
		 "unknown", "unit", "int", "float", "bool", "char", "ref", "array", "function", "custom", "constructor"
		};
		return graph_type_string[(int)(t)];
	}
    virtual llvm::Type* getLLVMType(llvm::Module *TheModule) = 0;
    virtual int getBound(){ wrongCall("getBound()"); exit(1);}
    virtual int *getBoundPtr(){ wrongCall("getBoundPtr()"); exit(1);}
    virtual void changeBoundVal(int newBound){ wrongCall("changeBoundVal()"); exit(1);}
    virtual void changeBoundPtr(int *newBoundptr){ wrongCall("changeBoundPtr()"); exit(1);}
    virtual void setDimensions(int newDims){ wrongCall("setDimensions()"); exit(1);}
    virtual std::vector<llvm::Type *> getLLVMParamTypes(llvm::Module *TheModule){ wrongCall("getLLVMParamTypes()"); exit(1);}
    virtual llvm::Type *getLLVMResultType(llvm::Module *TheModule){  wrongCall("getLLVMResultType()"); exit(1);}
    virtual ~TypeGraph() {}
};


class UnknownTypeGraph : public TypeGraph {
    unsigned long tmp_id;
    static unsigned long curr; //next temp
    bool can_be_array, can_be_func, only_int_char_float;
public:
    UnknownTypeGraph(bool can_be_array = false, bool can_be_func = false,bool only_int_char_float = false);
    std::string makeTypeString() override { return getTmpName();}
    std::string makeTypeString2() override { return getTmpName();}
    unsigned long getId() override {return tmp_id;  }
    std::string getTmpName() override{return "@" + std::to_string(tmp_id);}
    bool canBeArray() override{ return can_be_array; }
    bool canBeFunc() override{ return can_be_func; }
    bool onlyIntCharFloat() override{ return only_int_char_float; }
    void setIntCharFloat() override	{ only_int_char_float=true; }
    bool equals(TypeGraph *o) override {return o->isUnknown() && tmp_id == o->getId();}
    void copyConstraintFlags(TypeGraph *o) override{
		if (!o->isUnknown()) {
			log("Unknown Type of Arguments for: copyConstraintFlags() ");
			exit(1);
		}
		can_be_array = can_be_array && o->canBeArray();
		can_be_func = can_be_func && o->canBeFunc();
		only_int_char_float = only_int_char_float || o->onlyIntCharFloat();
	}
    virtual llvm::Type* getLLVMType(llvm::Module *TheModule) override;
    ~UnknownTypeGraph() {}
};


class BasicTypeGraph : public TypeGraph {
public:
    BasicTypeGraph(graphType t): TypeGraph(t) {}
    bool equals(TypeGraph *o){
		if (this == o) return true;
		return getSubClass() == o->getSubClass();
	}
    virtual ~BasicTypeGraph() {}
};

class UnitTypeGraph : public BasicTypeGraph {
public:
    UnitTypeGraph(): BasicTypeGraph(graphType::TYPE_unit) {}
    virtual llvm::Type* getLLVMType(llvm::Module *TheModule) override;
    ~UnitTypeGraph() {}
};

class IntTypeGraph : public BasicTypeGraph {
public:
    IntTypeGraph(): BasicTypeGraph(graphType::TYPE_int) {}
    virtual llvm::IntegerType* getLLVMType(llvm::Module *TheModule) override;
    ~IntTypeGraph() {}
};

class CharTypeGraph : public BasicTypeGraph {
public:
    CharTypeGraph(): BasicTypeGraph(graphType::TYPE_char) {}
    virtual llvm::IntegerType* getLLVMType(llvm::Module *TheModule) override;
    ~CharTypeGraph() {}
};

class BoolTypeGraph : public BasicTypeGraph {
public:
    BoolTypeGraph(): BasicTypeGraph(graphType::TYPE_bool) {}
    virtual llvm::IntegerType* getLLVMType(llvm::Module *TheModule) override;
    ~BoolTypeGraph() {}
};

class FloatTypeGraph : public BasicTypeGraph {
public:
    FloatTypeGraph(): BasicTypeGraph(graphType::TYPE_float) {}
    virtual llvm::Type* getLLVMType(llvm::Module *TheModule) override;
    ~FloatTypeGraph() {}
};

class ArrayTypeGraph : public TypeGraph {
    TypeGraph *Type;
    int dimensions;
    int *lowBound;
    std::string makeStringDimensions(){
		if (dimensions == 1) { return "Array of: ";} 
		else if (dimensions == -1) { return "Array at least " + std::to_string(*lowBound) + " of: ";}
		else {
			int temp = dimensions - 1;
			std::string retVal = "[*";
			do {
				retVal.append(", *");
			} while (temp -= 1);
			retVal.push_back(']');
			return "Array " + retVal + " of: ";
		}
	}
public:
    ArrayTypeGraph(int dimensions, TypeGraph *containedType, int lowBound = -1)
	: TypeGraph(graphType::TYPE_array), Type(containedType),dimensions(dimensions),lowBound(new int(lowBound)) {}
	~ArrayTypeGraph(){ 
		if (Type->isDeletable()) delete Type; 
		delete lowBound;
	}
    std::string makeTypeString() override{ return makeTypeString2() ;	}
    std::string makeTypeString2() override{ return std::string("(") + makeStringDimensions() +" "+getContainedType()->makeTypeString2() + std::string(")");}
    TypeGraph* getContainedType(){ return Type; }
    bool equals(TypeGraph *o){
		if (this == o) return true;
		return o->isArray() &&  getContainedType()->equals(o->getContainedType());
	}
    int getDimensions() override { return *lowBound != -1 && dimensions != -1 ? *lowBound : dimensions;}
    int getBound() override	{ return *lowBound; }
    int *getBoundPtr() override { return lowBound; }
    void changeBoundVal(int newBound) override{
		dimensions = -1; 
		*lowBound = newBound;
	}
    void changeBoundPtr(int *newBoundptr) override{
		dimensions = -1; 
		lowBound = newBoundptr;
	}
    void setDimensions(int newDims) override{
		 dimensions = newDims; 
		*lowBound = newDims; 
	}
    void changeInner(TypeGraph *replacement, unsigned int index = 0) override { Type = replacement;}
    virtual llvm::PointerType* getLLVMType(llvm::Module *TheModule) override;
};

class RefTypeGraph : public TypeGraph {
    TypeGraph *Type;
public:
    RefTypeGraph(TypeGraph *refType): TypeGraph(graphType::TYPE_ref), Type(refType) {}
    std::string makeTypeString() override {return makeTypeString2();}
    std::string makeTypeString2() override {return "Reference of: " + getContainedType()->makeTypeString2();}
    TypeGraph* getContainedType() override 	{ return Type; }
    bool equals(TypeGraph *o) override {
		if (this == o) return true;
		return o->isRef() &&  getContainedType()->equals(o->getContainedType());
	}
    void changeInner(TypeGraph *replacement, unsigned int index = 0) override { Type = replacement;}
    virtual llvm::PointerType* getLLVMType(llvm::Module *TheModule) override;
    ~RefTypeGraph(){ if (Type->isDeletable()) delete Type; }
};

class FunctionTypeGraph : public TypeGraph {
    std::vector<TypeGraph *> *paramTypes; 
    TypeGraph *resultType;
    std::string makeStringParams(){
		if (getParamCount() == 0) {	return "is Function";}
		std::string retVal = getParamType(0)->makeTypeString2();
		std::string temp;
		if (getParamType(0)->isFunction()) {
			retVal.push_back(')');
			retVal.insert(retVal.begin(), '(');
		}
		for (int i = 0; i < getParamCount(); i++) {
			if (i == 0) continue;
			temp = getParamType(i)->makeTypeString2();
			if (getParamType(1)->isFunction()) {
				temp.push_back(')');
				temp.insert(temp.begin(), '(');
			}
			retVal.append(" -> " + temp);
		}
		return retVal;
	}
public:
    FunctionTypeGraph(TypeGraph *resultType): 
		TypeGraph(graphType::TYPE_function), paramTypes(new std::vector<TypeGraph *>()),resultType(resultType) {}
    std::string makeTypeString() override	{ return makeTypeString2() ;}
    std::string makeTypeString2() override { return makeStringParams() + " -> " + getResultType()->makeTypeString2();}
    std::vector<TypeGraph *>* getParamTypes() override { return paramTypes;}
    TypeGraph* getResultType() override {return resultType;}
    int getParamCount() override { return paramTypes->size();}
    void addParam(TypeGraph *param, bool push_back = true) override{
		if (push_back)paramTypes->push_back(param);
		else paramTypes->insert(paramTypes->begin(), param);
	}
    TypeGraph* getParamType(unsigned int index) override{
		if (index >= paramTypes->size()) {
			std::cout << "Parameters exceed number of function arguments. Now aborting." << std::endl;
			exit(1);
		}
		return (*paramTypes)[index];
	}
    bool equals(TypeGraph *o) override{
		if (this == o) return true;
		if (o->isFunction() && getParamCount() == o->getParamCount()) {
			for (int i = 0; i < getParamCount(); i++) {
				if (!getParamType(i)->equals(o->getParamType(i)))
					return false;
			}
			return getResultType()->equals(o->getResultType());
		}
		return false;
	}
    void changeInner(TypeGraph *replacement, unsigned int index = 0) override {
		if (index > paramTypes->size()) {
			std::cout << "Parameters exceed number of function arguments. Now aborting." << std::endl;
			exit(1);
		} 
		else if (index == paramTypes->size()) {
			resultType = replacement;
		} 
		else {(*paramTypes)[index] = replacement;}
	}
	~FunctionTypeGraph() {
		for (auto &paramType: *paramTypes)
			if (paramType->isDeletable()) delete paramType;
		if (resultType->isDeletable()) delete resultType;
	}
    std::vector<llvm::Type *> getLLVMParamTypes(llvm::Module *TheModule) override;
    llvm::Type *getLLVMResultType(llvm::Module *TheModule) override;
    virtual llvm::PointerType* getLLVMType(llvm::Module *TheModule) override;
};

class ConstructorTypeGraph : public TypeGraph {
    CustomTypeGraph *customType;
    std::string name;
    std::vector<TypeGraph *> *fields;
    int index = -1; 
public:
    ConstructorTypeGraph(std::string name):TypeGraph(graphType::TYPE_record),customType(nullptr), name(name),fields(new std::vector<TypeGraph *>()) {}
    std::string makeTypeString() override { return name;}
    std::string makeTypeString2() override {return name;}
    std::vector<TypeGraph *>* getFields() override { return fields; }
    void addField(TypeGraph *field) override { fields->push_back(field); }
    void setTypeGraph(CustomTypeGraph *owningType) override { customType = owningType; }
    CustomTypeGraph* getCustomType() override { return customType; }
    int getFieldCount() override { return fields->size(); }
    TypeGraph* getFieldType(unsigned int index) override {
		if (index >= fields->size()) {
			std::cout << "Constructor number exceeds expected arguments. Now aborting." << std::endl;
			exit(1);
		}
		return (*fields)[index];
	}
    bool equals(TypeGraph *o) override;
    int getIndex();
    std::string getName() { return name; }
    virtual llvm::StructType* getLLVMType(llvm::Module *TheModule) override;
    ~ConstructorTypeGraph();
};
class CustomTypeGraph : public TypeGraph {
    std::string name;
    std::vector<ConstructorTypeGraph *> *constructors;
    llvm::Function *structEqFunc;
public:
    CustomTypeGraph(std::string name, std::vector<ConstructorTypeGraph *> *constructors = new std::vector<ConstructorTypeGraph *>())
	: TypeGraph(graphType::TYPE_custom), name(name), constructors(constructors), structEqFunc(nullptr) {}
    std::string makeTypeString() override { return name;}
    std::string makeTypeString2() override {return name;}
    std::vector<ConstructorTypeGraph *>* getConstructors() override { return constructors;}
    int getConstructorCount() override { return constructors->size();}
    void addConstructor(ConstructorTypeGraph *constructor) override{
		constructors->push_back(constructor);
		constructor->setTypeGraph(this);
	}
    bool equals(TypeGraph *o) override{
		if (o->isConstructor()) return this == o->getCustomType();
		return this == o;
	}
    int getConstructorIndex(ConstructorTypeGraph *c){
		for(long unsigned int i = 0; i < constructors->size(); i++)
		{
			if((*constructors)[i]->equals(c)) { return i;}
		}
		exit(1);
	}
    int getConstructorIndex(std::string Id){
		std::string s;
		for(long unsigned int i = 0; i < constructors->size(); i++)
		{
			s = (*constructors)[i]->getName();
			if(Id == s) { return i;}
		}
		exit(1);
	}
    virtual llvm::PointerType* getLLVMType(llvm::Module *TheModule) override;
    llvm::Function* getStructEqFunc(llvm::Module *TheModule,llvm::legacy::FunctionPassManager *TheFPM);
    ~CustomTypeGraph()	{ for (auto &constructor: *constructors)if (constructor->isDeletable()) delete constructor;}
};

#endif