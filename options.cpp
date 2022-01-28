#include <iostream>
#include <fstream>

#include "options.hpp"
#include "ast.hpp"
#include "parser.hpp"

const std::string type_string[] = {"unit", "int", "float", "bool", "char"};

static std::string fileNAME2;
static int spacesNeeded = 0;
const int constSpa = 2;
// 130 to be above ASCII 
int BOption::count = 130;

OptionsList optionsList;

Option::Option(int val, std::string name, std::string description, int has_arg)
    : val(val), name(name), description(description), has_arg(has_arg) {}
	
SOption::SOption(char c, std::string description)
    : Option(c, std::string(1, c), description) { optionsList.addSOption(this); }

BOption::BOption(std::string name, std::string description, int has_arg)
    : Option(count++, name, description, has_arg) { optionsList.addBOption(this); }


BOption		optimisation("O", "For optimization on the code produced");
BOption   	finalCode("f", "For printing assembly final code at the standard output");
BOption   	intermediate("i", "For printing LLVM intermediate code at the standard output");
BOption    	execfile("e", "For making executable file the argument following", required_argument);
BOption     astree("a", "For printing Abstract Syntax Tree at the standard output");
BOption     compiledDone("d", "For printing Succsess if compile succeeds");
BOption 	nothing(" ", "Creates executable exec.out, intermediate [filename].imm  and final [filename].asm ");
BOption     help("help", "For printing all available options and their description");

OptionsList::OptionsList()
{
    sOptions = {};
    bOptions = {};
}

void OptionsList::parseOptions(int argc, char **argv)
{
    if (argc == 1) 
    {
        std::cerr << "No arguments given. Aborting." << std::endl;
        exit(1);
    }

    fileNAME2= std::string(argv[1]);
    int index = 0, c;
    struct option *long_options = getBOptionArray();
    std::string short_options = "";
    while ((c = getopt_long_only(argc, argv, short_options.c_str(), long_options, &index)) != -1)
    {
        for (auto l : bOptions)
        {
            if (c == l->getVal())
            {
                l->activate();
                if (optarg) l->setOptarg(optarg);
                break;
            }
        }
        index = 0;
    }
		
    if (help.isActivated())
    {
        std::cout << "Avalaible options for ./llama  :" << std::endl <<std::endl;       
        for (auto l : bOptions)
        {
			if (l->getName()==" ")std::cout << "  " << std::left << std::setfill(' ') << std::setw(20) <<"(nothing)" ;
			else std::cout << "  " << std::left << std::setfill(' ') << std::setw(20) << "-" + l->getName();
            std::cout << std::left << l->getDescription()<< std::endl;
        }

        std::cout << std::endl << std::endl;
        exit(0);
    }
    
    if(std::freopen(fileNAME2.c_str(), "r", stdin) == nullptr) 
    {
        std::cerr << "Couldn't open file " << fileNAME2 << std::endl;
        exit(1);
    }
} 

void OptionsList::putXtraProgram(Program *p)
{
    this->p = p;
}

void OptionsList::executeOptions()
{
    std::string filename = "";
    if(execfile.isActivated())
    {
        filename = execfile.getOptarg();
        if(filename == "") 
        {
            std::cout << "No file given" << std::endl;
            exit(1);
        }
        if(!std::freopen(filename.c_str(), "w", stdout)) exit(1);
    }
	
	
	
    bool syntax, sem, inference, compile, link;
	syntax = sem = inference = compile = link = true;

    if (syntax)
    {
        if (astree.isActivated())
        {
            std::cout << "AST" << std::endl;
            std::cout << *p << std::endl;
        }
    }
	
    if (sem)
    {
        p->sem();
    }
	
    if (inference)
    {
        bool infSuccess = inf.solveAll(false);        
        if (!infSuccess) exit(1);
    }
	
    if (compile)
    {   
        p->scopeLive(nullptr); 
        bool opt = optimisation.isActivated();
        p->start_compilation("optimization.lla", opt);
		//when all are not activated
		if(	!help.isActivated() && !finalCode.isActivated() && !intermediate.isActivated()
			&& !execfile.isActivated() && !astree.isActivated() && !compiledDone.isActivated())
		{
			int endofslash = -1;
			endofslash = fileNAME2.find_last_of('/');
			int startofslash = -1;
			startofslash = fileNAME2.substr(0,endofslash).find_last_of('/');
			int dot = fileNAME2.size();
			int dot2 = fileNAME2.find_last_of('.');
			if(dot2 > endofslash) dot=dot2;
			std::string folder , file, pathtogive;
			if(endofslash==startofslash && startofslash==-1){
				folder="";
				file = fileNAME2.substr(0,dot);
			}
			else
			{
				file = fileNAME2.substr(endofslash+1,dot-endofslash-1);
				folder = fileNAME2.substr(startofslash+1,endofslash-startofslash-1);
			}
			pathtogive = file;
			const char *immPath2 = (pathtogive + ".imm").c_str();
			const char *asmPath2 = (pathtogive + ".asm").c_str();
			p->emitLLVMIC(immPath2);
			p->emitAssemblyCode(asmPath2);
			if (folder!="")
			{
				pathtogive=fileNAME2.substr(0,endofslash+1);
				std::string ah="mv ./" + std::string(asmPath2) + " " + pathtogive + std::string(asmPath2);
				if(std::system(ah.c_str()));	
				ah="mv ./" + std::string(immPath2) + " " + pathtogive + std::string(immPath2);
				if(std::system(ah.c_str()));	
			}
			std::string outA="exec.out";
			filename=outA;
			if(!std::freopen(outA.c_str(), "w", stdout)){
				std::cerr <<"Problem writing a.out"<<std::endl;
				exit(1);
			}
			//exit(0);
			
		}  
		
		
        if (intermediate.isActivated())
        {
            p->printLLVMIR();
        }
        if (finalCode.isActivated())
        {
            p->emitAssemblyCode();
        }
        
    }
    if (link)
    {
        if(!finalCode.isActivated()) 
        {
            p->emitObjectCode("a.o");
        }

#define XSTR(s) STR(s)
#define STR(s) #s
        std::string linkCommand = std::string("clang -o ") + 
                                  (filename == "" ? "a.out" : filename.c_str()) + std::string(" ") + "a.o " + 
#ifdef LIBLLAMA
                std::string(XSTR(LIBLLAMA)) +
#else
#error Location of llama runtime library must be specified
#endif 
                " " +
#ifdef LIBGC
                std::string(XSTR(LIBGC));
#else
                std::string("");
#endif
        if(std::system(linkCommand.c_str())) exit(1);
        if(!finalCode.isActivated()) 
        {
            if(std::system("rm a.o"))
                exit(1);
        }
    }
}

void createBlock(std::ostream &out)
{
    spacesNeeded += constSpa;
    out << std::endl;
}

void closeBlock(std::ostream &out)
{
    spacesNeeded -= constSpa;
}

void printHeader(std::ostream &out, std::string h)
{
    out << std::string(spacesNeeded, ' ') << '-' << h;
}
void printType(std::ostream &out, std::string t);

std::ostream &operator<<(std::ostream &out, const AST &t)
{
    t.printOn(out);
    return out;
}

std::string UnknownType::getTypeStr() const
{
    return TG->makeTypeString2();
}
std::string BasicType::getTypeStr() const
{
    return type_string[static_cast<int>(t)];
}
std::string FunctionType::getTypeStr() const
{
    return lhtype->getTypeStr() + "->" + rhtype->getTypeStr();
}
std::string ArrayType::getTypeStr() const
{
    return std::to_string(dimensions) + "D Array of: " + elem_type->getTypeStr();
}
std::string RefType::getTypeStr() const
{
    return "Reference of: " + ref_type->getTypeStr();
}
std::string CustomType::getTypeStr() const
{
    return id;
}

void Type::printOn(std::ostream &out) const
{
    printHeader(out, "Type: " + getTypeStr());
    createBlock(out);
    closeBlock(out);
}


void Constr::printOn(std::ostream &out) const
{
    printHeader(out, "Constr: " + Id);
    createBlock(out);
    for (auto *t : type_list)
    {
        out << *t;
    }
    closeBlock(out);
}
void Par::printOn(std::ostream &out) const
{
    printHeader(out, "Par: " + id);
    createBlock(out);
    out << *T;
    closeBlock(out);
}

void Tdef::printOn(std::ostream &out) const
{
    printHeader(out, "Tdef: " + id);
    createBlock(out);
    for (Constr *c : constr_list)
    {
        out << *c;
    }
    closeBlock(out);
}
void Constant::printOn(std::ostream &out) const
{
    printHeader(out, "Constant: " + id);
    createBlock(out);
    out << *T;
    out << *expr;
    closeBlock(out);
}
void Function::printOn(std::ostream &out) const
{
    printHeader(out, "Function: " + id);
    createBlock(out);
    out << *T;
    printHeader(out, "Parameters: ");
    createBlock(out);
    for (Par *p : par_list)
    {
        out << *p;
    }
    closeBlock(out);
    printHeader(out, "Body: ");
    createBlock(out);
    out << *expr;
    closeBlock(out);
    closeBlock(out);
}
void Array::printOn(std::ostream &out) const
{
    printHeader(out, "Array: " + id);
    createBlock(out);
    out << *T;
    for (auto *e : expr_list)
    {
        out << *e;
    }
    closeBlock(out);
}
void Variable::printOn(std::ostream &out) const
{
    printHeader(out, "Variable: " + id);
    createBlock(out);
    out << *T;
    closeBlock(out);
}

void Letdef::printOn(std::ostream &out) const
{
    if (recursive)
        printHeader(out, "Letdef rec: ");
    else
        printHeader(out, "Letdef: ");

    createBlock(out);
    for (auto *d : def_list)
    {
        out << *d;
    }
    closeBlock(out);
}
void Typedef::printOn(std::ostream &out) const
{
    printHeader(out, "Typedef: ");
    createBlock(out);
    for (auto *t : tdef_list)
    {
        out << *t;
    }
    closeBlock(out);
}
void Program::printOn(std::ostream &out) const
{
    printHeader(out, "Program: ");
    createBlock(out);
    for (auto *d : definition_list)
    {
        out << *d;
    }
    closeBlock(out);
}

void LetIn::printOn(std::ostream &out) const
{
    printHeader(out, "LetIn: ");
    createBlock(out);
    out << *letdef;
    out << *expr;
    closeBlock(out);
}

void String_Const::printOn(std::ostream &out) const
{
    printHeader(out, "String_Const: " + originalStr);
    createBlock(out);
    closeBlock(out);
}
void Char_Const::printOn(std::ostream &out) const
{
    printHeader(out, "Char_Const: " + c_string);
    createBlock(out);
    closeBlock(out);
}
void Bool_Const::printOn(std::ostream &out) const
{
    printHeader(out, "Bool_Const: " + std::to_string(b));
    createBlock(out);
    closeBlock(out);
}
void Float_Const::printOn(std::ostream &out) const
{
    printHeader(out, "Float_Const: " + std::to_string(d));
    createBlock(out);
    closeBlock(out);
}
void Int_Const::printOn(std::ostream &out) const
{
    printHeader(out, "Int_Const: " + std::to_string(n));
    createBlock(out);
    closeBlock(out);
}
void Unit::printOn(std::ostream &out) const
{
    printHeader(out, "Unit: ");
    createBlock(out);
    closeBlock(out);
}

std::string opToString(int op)
{
    switch (op)
    {
        case '!':
        case '+':
        case '-': 
        case '*':
        case '/':
        case '=':
        case '<':
        case '>':
        case ';':           return std::string(1, char(op));
        case T_mod:         return "mod";
        case T_plusdot:     return "+.";
        case T_mindot:    	return "-.";
        case T_stardot:     return "*.";
        case T_sldot:    	return "/.";
        case T_starstar:    return "**";
        case T_barbar:      return "||";
        case T_andand:		return "&&";
        case T_lgr: 		return "<>";
        case T_eqeq:       	return "==";
        case T_excleq:    	return "!=";
        case T_leq:         return "<=";
        case T_greq:        return ">=";
        case T_coleq:     	return ":=";
        case T_not:         return "not";
        case T_delete:      return "delete";
        default:            return ""; break;
    }
}
void BinOp::printOn(std::ostream &out) const
{
    std::string opStr = opToString(op);
    if(opStr == "") printHeader(out, "BinOp: " + std::to_string(op));
    else printHeader(out, "BinOp: " + opStr);

    createBlock(out);
    out << *lhs;
    out << *rhs;
    closeBlock(out);
}
void UnOp::printOn(std::ostream &out) const
{
    std::string opStr = opToString(op);
    if(opStr == "") printHeader(out, "UnOp: " + std::to_string(op));
    else printHeader(out, "UnOp: " + opStr);

    createBlock(out);
    out << *expr; 
    closeBlock(out);
}
void New::printOn(std::ostream &out) const
{
    printHeader(out, "New: ");
    createBlock(out);
    out << *new_type;
    closeBlock(out);
}

void While::printOn(std::ostream &out) const
{
    printHeader(out, "While: ");
    createBlock(out);
    out << *cond;
    out << *body;
    closeBlock(out);
}
void For::printOn(std::ostream &out) const
{
    printHeader(out, "For: " + id);
    createBlock(out);
    out << *start;
    printHeader(out, step);
    out << *finish;
    out << *body;
    closeBlock(out);
}
void If::printOn(std::ostream &out) const
{
    printHeader(out, "If: ");
    createBlock(out);
    out << *cond;
    out << *body;
    if (else_body)
        out << *else_body;
    closeBlock(out);
}

void Dim::printOn(std::ostream &out) const
{
    printHeader(out, "Dim: ");
    createBlock(out);
    if (dim)
        out << *dim;
    out << id;
    closeBlock(out);
}

void ConstantCall::printOn(std::ostream &out) const
{
    printHeader(out, "ConstantCall: " + id);
    createBlock(out);
    closeBlock(out);
}
void FunctionCall::printOn(std::ostream &out) const
{
    printHeader(out, "FunctionCall: " + id);
    createBlock(out);
    for (auto *e : expr_list)
    {
        out << *e;
    }
    closeBlock(out);
}
void ConstrCall::printOn(std::ostream &out) const
{
    printHeader(out, "ConstrCall: " + Id);
    createBlock(out);
    for (auto *e : expr_list)
    {
        out << *e;
    }
    closeBlock(out);
}
void ArrayAcc::printOn(std::ostream &out) const
{
    printHeader(out, "ArrayAcc: " + id);
    createBlock(out);
    for (auto *e : expr_list)
    {
        out << *e;
    }
    closeBlock(out);
}

void Pattern_Const::printOn(std::ostream &out) const
{
    printHeader(out, "Pattern_Const: ");

    createBlock(out);
    out << *literal;
    closeBlock(out);
}
void PatternId::printOn(std::ostream &out) const
{
    printHeader(out, "PatternId: " + id);
    createBlock(out);
    closeBlock(out);
}
void PatternConstr::printOn(std::ostream &out) const
{
    printHeader(out, "PatternConstr: " + Id);
    createBlock(out);
    for (Pattern *p : pattern_list)
    {
        out << *p;
    }
    closeBlock(out);
}

void Clause::printOn(std::ostream &out) const
{
    printHeader(out, "Clause: ");
    createBlock(out);
    printHeader(out, "Pattern: ");
    createBlock(out);
    out << *pattern;
    closeBlock(out);
    printHeader(out, "Expression: ");
    createBlock(out);
    out << *expr;
    closeBlock(out);
    closeBlock(out);
}
void Match::printOn(std::ostream &out) const
{
    printHeader(out, "Match:");
    createBlock(out);
    for (Clause *c : clause_list)
    {
        out << *c;
    }
    closeBlock(out);
}
