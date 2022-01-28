#include <iostream>
#include "types.hpp"
#include "type_inference.hpp"
#include "ast.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>


unsigned long UnknownTypeGraph::curr = 1;

UnknownTypeGraph::UnknownTypeGraph(bool can_be_array, bool can_be_func, bool only_int_char_float):
TypeGraph(graphType::TYPE_unknown), tmp_id(curr++),can_be_array(can_be_array), can_be_func(can_be_func), only_int_char_float(only_int_char_float) {
    inf.initReplacement(getTmpName());
}

bool ConstructorTypeGraph::equals(TypeGraph *o) {
    if (this == o) return true;
    return (o->isConstructor() || o->isCustom()) && getCustomType()->equals(o);
}

int ConstructorTypeGraph::getIndex(){
	if(index == -1) index = customType->getConstructorIndex(name);
	return index;
}

ConstructorTypeGraph::~ConstructorTypeGraph() {
	for (auto &field: *fields) if (field->isDeletable())delete field;
	if (customType->isDeletable()) delete customType;		
}
