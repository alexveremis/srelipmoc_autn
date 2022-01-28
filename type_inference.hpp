#ifndef __TYPE_INFERENCE_HPP__
#define __TYPE_INFERENCE_HPP__

#include <string>
#include <vector>
#include <map>
#include <functional>
#include <iostream>
#include <algorithm>
#include "types.hpp"

class Constraint {
    TypeGraph *lhs, *rhs;
    int lineno;
    std::string msg;
public:
    Constraint(TypeGraph *lhs, TypeGraph *rhs, int lineno): lhs(lhs), rhs(rhs), lineno(lineno), errCallback(nullptr) {}
    TypeGraph* getLhs(){ return lhs; }
    TypeGraph* getRhs(){ return rhs; }
    std::function<void(void)> *errCallback; 
    void setErrCallback(std::function<void(void)> *callback){ errCallback = callback;}
    int getLineNo() { return lineno; }
    std::string makeString();
    virtual ~Constraint(){
		delete lhs;
		delete rhs;
	}
};

class typeInferer {
    std::vector<Constraint *> *constraints;
    std::map<std::string, TypeGraph *> *replacements;
    bool isValidReplacement(TypeGraph *unknownType, TypeGraph *possibleType){
		bool invalid = (
			(!unknownType->canBeArray() && possibleType->isArray()) ||
			(!unknownType->canBeFunc() && possibleType->isFunction()) ||
			(unknownType->onlyIntCharFloat() && !possibleType->isUnknown() && !possibleType->isInt() && !possibleType->isChar() && !possibleType->isFloat()));
		return !invalid;
	}
    TypeGraph* isUnknownExistsHasReplacement(TypeGraph *unknownType){
		if (!unknownType->isUnknown()) return nullptr;
		auto itr = replacements->find(unknownType->getTmpName());
		if (itr == replacements->end()) error("Internal compiler error. Type name was not found in hash map");
		return itr->second;
	}
    void saveReplacement(std::string unknownTmpName, TypeGraph *resolvedTypeGraph){
		auto itr = replacements->find(unknownTmpName);
		if (itr == replacements->end()) error("Internal compiler error. Type name: " + unknownTmpName + " was not found in hash map");
		if (itr->second != nullptr) error("Internal compiler error. Type name: " + unknownTmpName + " cannot be replaced multiple times");
		itr->second = resolvedTypeGraph;
	}
    void tryReplace(TypeGraph *unknownType, TypeGraph *possibleType, int lineno){
		if (debug)
			log("Replacing the Type with name: " + unknownType->makeTypeString() +
				" at line: " + std::to_string(lineno) +
				" with: " + possibleType->makeTypeString());
		if (!isValidReplacement(unknownType, possibleType)) 
			error("Tried replacing at line: " + std::to_string(lineno) + " and failed due to constraint violation");
		if (occurs(unknownType, possibleType)) 
			error("At line: " + std::to_string(lineno) + " exists constraint with recursive unknown type");
		if (possibleType->isUnknown())	possibleType->copyConstraintFlags(unknownType);
		saveReplacement(unknownType->getTmpName(), possibleType);
	}
    bool occurs(TypeGraph *unknownType, TypeGraph *possibleType){
		if (possibleType->isArray() || possibleType->isRef()) {return isOrOccurs(unknownType, tryApplyReplacements(possibleType->getContainedType()));}
		else if (possibleType->isFunction()) { 
			for (int i = 0; i < possibleType->getParamCount(); i++) {
				if (isOrOccurs(unknownType, tryApplyReplacements(possibleType->getParamType(i)))) return true; 
	  		}
			return isOrOccurs(unknownType, tryApplyReplacements(possibleType->getResultType()));
		} 
		else {return false; }// return false in all others cases of uknown Type 
	}
    bool isOrOccurs(TypeGraph *unknownType, TypeGraph *possibleType){ return unknownType->equals(possibleType)|| occurs(unknownType, possibleType);}
    bool findingConstraints(Constraint *constraint);
    void log(std::string msg){std::cout << "Type Inferer: " << msg << std::endl;}
    bool debug;
public:
    typeInferer(bool debug = false): constraints(new std::vector<Constraint *>()),replacements(new std::map<std::string, TypeGraph *>()), debug(debug) {}
    void error(std::string msg){
		log("Error: " + msg);
		exit(1);
	}
    std::vector<Constraint *>* getConstraints(){ return constraints; }
    std::map<std::string, TypeGraph *>* getReplacements()	{ return replacements;}
    TypeGraph* getReplacedLhs(Constraint *constraint)	{ return tryApplyReplacements(constraint->getLhs());}
    TypeGraph* getReplacedRhs(Constraint *constraint)	{ return tryApplyReplacements(constraint->getRhs());}
    TypeGraph* getReplacedType(TypeGraph *unknownType)	{ return tryApplyReplacements(unknownType);}
    TypeGraph* tryApplyReplacements(TypeGraph* unknownType){
		std::vector<std::string> toChange;
		TypeGraph *next, *current = unknownType;
		while ((next = isUnknownExistsHasReplacement(current))){
			toChange.push_back(current->getTmpName());
			current = next;
		}
		for (auto &name: toChange) { 
			(*replacements)[name] = current; //replace all found
		}
		return current;
	}
    TypeGraph* deepReplace(TypeGraph* unknownType){
    TypeGraph *temp = tryApplyReplacements(unknownType);
		if (!temp->isFunction() && !temp->isArray() && !temp->isRef()) {return temp;} 
		else if (temp->isArray() || temp->isRef()) {
			temp->changeInner(deepReplace(temp->getContainedType()));
			return temp;
		} 
		else { 
			for (int i = 0; i < temp->getParamCount(); i++) {
				temp->changeInner(deepReplace(temp->getParamType(i)), i);
			}
			temp->changeInner(deepReplace(temp->getResultType()), temp->getParamCount());
			return temp;
		}
	}
    bool solveAll(bool err = true){
		std::reverse(constraints->begin(), constraints->end());
		Constraint *tempConstraint;
		while(!constraints->empty()) {
			tempConstraint = constraints->back();
			constraints->pop_back();
			if (!findingConstraints(tempConstraint))
				return false;
		}
		return checkAllReplaced(err);
	}
   
    void addConstraint(TypeGraph *lhs, TypeGraph *rhs, int lineno,std::function<void(void)> *errCallback = nullptr){
		if (debug)
			log("Adding the Constraint: " + lhs->makeTypeString()+ " == " + rhs->makeTypeString() + " at line: " + std::to_string(lineno));
		lhs = tryApplyReplacements(lhs);
		rhs = tryApplyReplacements(rhs);
		auto newConstraint = new Constraint(lhs, rhs, lineno);
		if (!errCallback) {
			errCallback = new std::function<void(void)>(
				[=]() { log("Fail at the Constraint: " + newConstraint->makeString());});
		}
		newConstraint->setErrCallback(errCallback);
		constraints->push_back(newConstraint);
	}
    void initReplacement(std::string name)	{ replacements->insert({name, nullptr});}
    bool checkAllReplaced(bool err = true){
		bool success = true;
		if(debug) log("Now Checking if/the completed Type Inference of all Unknown Types");
		for (auto &pair: (*replacements)) {
			if (!pair.second || tryApplyReplacements(pair.second)->isUnknown()) {
				log("Type Inference of the Type: " + pair.first + " failed");
				if (err) exit(1);
				else success = false;
			} 
		}
		if (success) if(debug) log("Type Inference finished successfully");
		return success;
	}
    void enableLogs(){ debug = true; }
    ~typeInferer(){ 
		for (auto &pair: *constraints) {
			delete pair;
		}
		delete constraints;
	}
};

extern typeInferer inf;

#endif // __INFER_HPP__
