#include <iostream>
#include "type_inference.hpp"

typeInferer inf(false);

std::string Constraint::makeString() {
    return "(line " + std::to_string(lineno) + ") " + 
            inf.deepReplace(lhs)->makeTypeString() + 
            " == " + inf.deepReplace(rhs)->makeTypeString() +
            " (comes from " + lhs->makeTypeString() + " == " + rhs->makeTypeString() + ")";
}

bool arrayOrRefs(TypeGraph *a, TypeGraph *b) {
    
    if (a->isRef() && b->isRef()) {return true;} 
	else if (a->isArray() && b->isArray()) {
        if (a->getDimensions() != -1 && b->getDimensions() != -1 &&(a->getDimensions() == b->getDimensions())) {
            return true; // when known dims
        } 
		else if (a->getDimensions() == -1 && b->getDimensions() == -1) {
            if (a->getBound() >= b->getBound()) {
                b->changeBoundPtr(a->getBoundPtr()); // we keep the greatest array
            } else {
                a->changeBoundPtr(b->getBoundPtr());
            }
            return true; // match
        } else if (a->getDimensions() == -1) { //  a is unknown
            if (b->getDimensions() < a->getBound()) { //known dimensions with unknown 
                return false;
            } else { // fix'em when smvt
                a->setDimensions(b->getDimensions());
                return true;
            }
        } else if (b->getDimensions() == -1) {
            if (a->getDimensions() < b->getBound()) {
                return false;
            } else {
                b->setDimensions(a->getDimensions());
                return true;
            }
        } 
    }
    return false;
}

bool typeInferer::findingConstraints(Constraint *constraint) {
    TypeGraph *lhsTypeGraph = getReplacedLhs(constraint),*rhsTypeGraph = getReplacedRhs(constraint);
    if (debug) log("Examining the constraint: " + constraint->makeString());
    if (lhsTypeGraph->equals(rhsTypeGraph)) {
        if(debug) log("Constraint matches already found types, aborting.");    
    } else if (lhsTypeGraph->isUnknown()) {                                      
        tryReplace(lhsTypeGraph, rhsTypeGraph, constraint->getLineNo());
    } else if (rhsTypeGraph->isUnknown()) {                                        
        tryReplace(rhsTypeGraph, lhsTypeGraph, constraint->getLineNo());
    } else if (lhsTypeGraph->isFunction() && rhsTypeGraph->isFunction()) {         
        if (lhsTypeGraph->getParamCount() != rhsTypeGraph->getParamCount()) {   // number of params is not the same
            error("At line: " + std::to_string(constraint->getLineNo()) +" different number of parameters");     
         } else {      // params number is the same
            if (debug) log("Examining the constraints of " + lhsTypeGraph->makeTypeString() +
                    " and " + rhsTypeGraph->makeTypeString() +
                    " at line " + std::to_string(constraint->getLineNo()));
            for (int i = 0; i < lhsTypeGraph->getParamCount(); i++) {
                addConstraint(lhsTypeGraph->getParamType(i), rhsTypeGraph->getParamType(i),constraint->getLineNo(), constraint->errCallback); // examining param types
            }
            addConstraint(lhsTypeGraph->getResultType(), rhsTypeGraph->getResultType(),constraint->getLineNo(), constraint->errCallback); // adding constr for result type
        }
    } else if (arrayOrRefs(lhsTypeGraph, rhsTypeGraph)) { // both arrays or refs with same dimensions
        addConstraint(lhsTypeGraph->getContainedType(), rhsTypeGraph->getContainedType(), constraint->getLineNo(), constraint->errCallback);
    } else {  // other fails
        (*(constraint->errCallback))();
        return false;
    }
    return true;
}
