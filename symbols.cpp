#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "symbols.hpp"

//got it from .hpp
bool             t_logs = false;
TypeTable        tt(t_logs); 
SymbolTable      st(t_logs); 
ConstructorTable ct(t_logs);

bool nameInScope(std::string name, std::map<std::string, SymbolEntry *> *scope) {
    return (scope->find(name) != scope->end());
}

void SymbolTable::insertLibFunctions() {
    std::vector<TypeGraph *> basicTypes = {
        tt.searchingType("unit")->getTypeGraph(),
        tt.searchingType("int")->getTypeGraph(), 
        tt.searchingType("bool")->getTypeGraph(),
        tt.searchingType("char")->getTypeGraph(),
        tt.searchingType("float")->getTypeGraph()
    };
    TypeGraph *stringType = new ArrayTypeGraph(1, new RefTypeGraph(basicTypes[3]));
    std::string stringTypeName = "string";
    std::vector<std::string> names;
    std::transform(basicTypes.begin(), basicTypes.end(),
        std::back_inserter(names), [] (TypeGraph* const& graph) {
            return graph->makeTypeString2();
        }
    );
    basicTypes.push_back(stringType);
    names.push_back(stringTypeName);
    std::string prefix;
    TypeGraph *resType, *paramType, *funcType;
    for (int i = 1; i < 6; i++) { 
        for (int j = 0; j < 2; j++) {
            if (j) {
                prefix = "print_";
                resType = basicTypes[0];
                paramType = basicTypes[i];
            } else {
                prefix = "read_";
                resType = basicTypes[i];
                paramType = basicTypes[0];
            }
            if (!j && i == 5) { // read_string is special
                funcType = new FunctionTypeGraph(basicTypes[0]);
                funcType->addParam(basicTypes[5]);
            } else {
                funcType = new FunctionTypeGraph(resType);
                funcType->addParam(paramType);
            }
            insertBasic(prefix + names[i], funcType);
        }
    }
    TypeGraph *int_to_int = new FunctionTypeGraph(basicTypes[1]),
              *float_to_float = new FunctionTypeGraph(basicTypes[4]),
              *unit_to_float = new FunctionTypeGraph(basicTypes[4]),
              *int_to_float = new FunctionTypeGraph(basicTypes[4]),
              *float_to_int = new FunctionTypeGraph(basicTypes[1]),
              *char_to_int = new FunctionTypeGraph(basicTypes[1]),
              *int_to_char = new FunctionTypeGraph(basicTypes[3]),
              *int_ref_to_unit = new FunctionTypeGraph(basicTypes[0]),
              *arrchar_to_int = new FunctionTypeGraph(basicTypes[1]),
              *arrchar_arrchar_to_int = new FunctionTypeGraph(basicTypes[1]),
              *arrchar_arrchar_to_unit = new FunctionTypeGraph(basicTypes[0]);
    int_to_int->addParam(basicTypes[1]);
    float_to_float->addParam(basicTypes[4]);
    unit_to_float->addParam(basicTypes[0]);
    int_to_float->addParam(basicTypes[1]);
    float_to_int->addParam(basicTypes[4]);
    char_to_int->addParam(basicTypes[3]);
    int_to_char->addParam(basicTypes[1]);
    int_ref_to_unit->addParam(new RefTypeGraph(basicTypes[1]));
    arrchar_to_int->addParam(basicTypes[5]);
    arrchar_arrchar_to_int->addParam(basicTypes[5]);
    arrchar_arrchar_to_int->addParam(basicTypes[5]);
    arrchar_arrchar_to_unit->addParam(basicTypes[5]);
    arrchar_arrchar_to_unit->addParam(basicTypes[5]);
    insertBasic("abs", int_to_int);
    insertBasic("fabs", float_to_float);
    insertBasic("sqrt", float_to_float);
    insertBasic("sin", float_to_float);
    insertBasic("cos", float_to_float);
    insertBasic("tan", float_to_float);
    insertBasic("atan", float_to_float);
    insertBasic("exp", float_to_float);
    insertBasic("ln", float_to_float);
    insertBasic("pi", unit_to_float);
    insertBasic("incr", int_ref_to_unit);
    insertBasic("decr", int_ref_to_unit);
    insertBasic("float_of_int", int_to_float);
    insertBasic("int_of_float", float_to_int);
    insertBasic("round", float_to_int);
    insertBasic("int_of_char", char_to_int);
    insertBasic("char_of_int", int_to_char);
    insertBasic("strlen", arrchar_to_int);
    insertBasic("strcmp", arrchar_arrchar_to_int);
    insertBasic("strcpy", arrchar_arrchar_to_unit);
    insertBasic("strcat", arrchar_arrchar_to_unit);
}

SymbolEntry* SymbolTable::insert(SymbolEntry *entry, bool overwrite) {
    if (debug) 
        log("Inserting " + entry->getTypeGraph()->makeTypeString() + " with name " + entry->name);
    if (nameInScope(entry->name, Table->back()) && !overwrite) {
            return nullptr;
    } else {
        (*Table->back())[entry->name] = entry;
        return entry;
    }
}

SymbolEntry* SymbolTable::searching(std::string name, bool err) {
    if (debug)
        log("Searching name: " + name);
    for (auto it = Table->rbegin(); it != Table->rend(); ++it) {
        if (nameInScope(name, *it)) {
            return (**it)[name];
        }
    }
    if (debug)
        log("Symbol " + name + " was not found");
    return nullptr;
}


SymbolEntry* BaseTable::insert(SymbolEntry *entry, bool overwrite) {
    if (debug) log("Inserting " + entry->getTypeGraph()->makeTypeString() + " with name " + entry->name);
    if (nameInScope(entry->name, Table) && !overwrite) {
        std::string msg = "Name " + entry->name + " was declared at least twice";
        if (debug)log(msg);
        return nullptr;
    } else {
        (*Table)[entry->name] = entry;
        return entry;
    }
}
SymbolEntry* BaseTable::searching(std::string name, bool err) {
    if (debug) log("Searching name: " + name);
    if (nameInScope(name, Table)) { return (*Table)[name]; }
    if (debug) log("Name " + name + " was not found");
    return nullptr;
}


TypeEntry::~TypeEntry() { 
    for (auto &constructor: *constructors) 
        delete constructor; 
    delete constructors;
}
void TypeEntry::addConstructor(ConstructorEntry *constr) {    
    constructors->push_back(constr);
    constr->setTypeEntry(this);
    getTypeGraph()->addConstructor(
        dynamic_cast<ConstructorTypeGraph*>(constr->getTypeGraph())
    );
}

