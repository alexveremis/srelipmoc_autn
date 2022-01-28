%define parse.error verbose

%{
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <ctype.h>

#include "lexer.hpp"
#include "ast.hpp"
#include "options.hpp"

%}

%union {
    Program *program;
    Definition *definition;	
    DefStmt *def_stmt;
	Constr *constr;
	Par *par;
    Type *type;
	Expr *expr;
    Clause *clause;
	Pattern *pat;
	
	std::vector<DefStmt *> *defstmt_vect;
    std::vector<Constr *> *constr_vect;	
    std::vector<Par *> *par_vect;
    std::vector<Type *> *type_vect;
    std::vector<Expr *> *expr_vect;
    std::vector<Clause *> *clause_vect;
    std::vector<Pattern *> *pat_vect;
	
    std::string *id;
    int op;     
    int num;
    float fl;
    std::string *str;
    
}

%token T_and "and"
%token T_array "array"
%token T_begin "begin"
%token T_bool "bool"
%token T_char "char"
%token<op> T_delete "delete"
%token T_dim "dim"
%token T_do "do"
%token T_done "done"
%token T_downto "downto"
%token T_else "else"
%token T_end "end"
%token T_false "false"
%token T_float "float"
%token T_for "for"
%token T_if "if"
%token T_in "in"
%token T_int "int"
%token T_let "let"
%token T_match "match"
%token T_mod "mod"
%token T_mutable "mutable"
%token T_new "new"
%token<op> T_not "not"
%token T_of "of"
%token T_rec "rec"
%token T_ref "ref"
%token T_then "then"
%token T_to "to"
%token T_true "true"
%token T_type "type"
%token T_unit "unit"
%token T_while "while"
%token T_with "with"

%token<id> T_identifier 
%token<id> T_constructor //constructor with an Upper case letter

%token<num> T_int_const 
%token<fl> T_float_const 
%token<str> T_char_const 
%token<str> T_string_const

%token T_mingr "->"
%token T_plusdot "+."
%token T_mindot "-."
%token T_stardot "*."
%token T_sldot "/."
%token T_starstar "**"
%token T_andand "&&"
%token T_barbar "||"
%token T_lgr "<>"
%token T_leq "<="
%token T_greq ">="
%token T_eqeq "=="
%token T_excleq "!="
%token T_coleq ":="

// Associativity


// Operators
%precedence LET_IN
%left<op> ';'
%precedence "then" 
%precedence "else"
%nonassoc<op> ":="
%left<op> "||"
%left<op> "&&"
%nonassoc<op> '=' "<>" '>' '<' "<=" ">=" "==" "!=" COMPOP
%left<op> '+' '-' "+." "-." ADDOP
%left<op> '*' '/' "*." "/." "mod" MULTOP
%right<op> "**"
%precedence UNOPS

%right "->"
%precedence "ref"
%precedence ARRAY_OF

%type<program> program program_list
%type<definition> definition_choice letdef typedef
%type<def_stmt> def tdef
%type<par_vect> par_list
%type<expr_vect> br_comma_expr_list comma_expr_st_list expr_2_list
%type<pat_vect> pattern_list
%type<constr_vect> bar_constr_st_list
%type<defstmt_vect> and_def_st_list and_tdef_st_list
%type<constr> constr
%type<type_vect> of_type_list type_2_list
%type<par> par
%type<type> type
%type<op> unop comp_operator add_sub_operator mul_div_operator
%type<expr> expr expr_2
%type<num> comma_star_list br_star
%type<pat> pattern 
%type<clause_vect> clause_list
%type<clause> clause

%%
program:
  program_list                      { $$ = $1; optionsList.putXtraProgram($$); } 
;

program_list:
  %empty                            { $$ = new Program(); }
| program_list definition_choice    { $1->append($2); $$ = $1; }
;

definition_choice:               
  letdef                            { $$ = $1; }
| typedef                           { $$ = $1; }
;

letdef:
  "let" def and_def_st_list        { $3->insert($3->begin(), $2); $$ = new Letdef($3); }
| "let" "rec" def and_def_st_list  { $4->insert($4->begin(), $3); $$ = new Letdef($4, true); }
;

typedef:
  "type" tdef and_tdef_st_list     { $3->insert($3->begin(), $2); $$ = new Typedef($3); }
;

def:
  T_identifier '=' expr                                    	{ $$ = new Constant($1, $3); }
| T_identifier ':' type '=' expr                           	{ $$ = new Constant($1, $5, $3); }
| T_identifier par_list '=' expr                           	{ $$ = new Function($1, $2, $4); }
| T_identifier par_list ':' type '=' expr                  	{ $$ = new Function($1, $2, $6, $4); }
| "mutable" T_identifier br_comma_expr_list           		{ $$ = new Array($2, $3); }
| "mutable" T_identifier br_comma_expr_list ':' type  		{ $$ = new Array($2, $3, $5); }
| "mutable" T_identifier                                   	{ $$ = new Variable($2); }
| "mutable" T_identifier ':' type                          	{ $$ = new Variable($2, $4); }
;

par_list:
  par                           { $$ = new std::vector<Par *>(); $$->push_back($1); }
| par_list par                  { $1->push_back($2); $$ = $1; }
;



br_comma_expr_list:
  '[' expr comma_expr_st_list ']'  { $3->insert($3->begin(), $2); $$ = $3; }
;

comma_expr_st_list:
  %empty                            { $$ = new std::vector<Expr *>(); }
| comma_expr_st_list ',' expr      	{ $1->push_back($3); $$ = $1; }
;

tdef:
  T_identifier '=' constr bar_constr_st_list  { $4->insert($4->begin(), $3); $$ = new Tdef($1, $4); }
;

bar_constr_st_list:
 %empty                            { $$ = new std::vector<Constr *>(); }
| bar_constr_st_list '|' constr    { $1->push_back($3); $$ = $1; }
;

and_def_st_list:
  %empty                            { $$ = new std::vector<DefStmt *>(); }
| and_def_st_list "and" def        	{ $1->push_back($3); $$ = $1; }
;

and_tdef_st_list:
  %empty                            { $$ = new std::vector<DefStmt *>(); }
| and_tdef_st_list "and" tdef      	{ $1->push_back($3); $$ = $1; }
;

constr:
  T_constructor of_type_list        { $$ = new Constr($1, $2); }
;

of_type_list:
  %empty                            { $$ = new std::vector<Type *>(); }
| "of" type_2_list            		{ $$ = $2; }
;

type_2_list:
  type                              { $$ = new std::vector<Type *>(); $$->push_back($1); }
| type_2_list type            		{ $1->push_back($2); $$ = $1; }
;

par:
  T_identifier                         { $$ = new Par($1); }
| '(' T_identifier ':' type ')'        { $$ = new Par($2, $4); }
;

type:
  "unit"                            { $$ = new BasicType(type::TYPE_unit); }     
| "int"                             { $$ = new BasicType(type::TYPE_int); }
| "char"                            { $$ = new BasicType(type::TYPE_char); }
| "bool"                            { $$ = new BasicType(type::TYPE_bool); }
| "float"                           { $$ = new BasicType(type::TYPE_float); }
| '(' type ')'                      { $$ = $2; }
| type "->" type                    { $$ = new FunctionType($1, $3); }
| type "ref"                        { $$ = new RefType($1); }
| "array" br_star "of" type %prec ARRAY_OF  { $$ = new ArrayType($2, $4); }
| T_identifier                      { $$ = new CustomType($1); }
;

br_star:
  %empty                            { $$ = 1; }
| '[' '*' comma_star_list ']'   	{ $$ = 1 + $3; }
;

comma_star_list:
  %empty                            { $$ = 0; }
| comma_star_list ',' '*'       	{ $$ = 1 + $1; }
;

expr:
  letdef "in" expr %prec LET_IN         { $$ = new LetIn($1, $3); }
| expr ';' expr                         { $$ = new BinOp($1, $2, $3); }
| "if" expr "then" expr "else" expr     { $$ = new If($2, $4, $6); }
| "if" expr "then" expr                 { $$ = new If($2, $4); }
| expr ":=" expr                        { $$ = new BinOp($1, $2, $3); }
| expr "||" expr                        { $$ = new BinOp($1, $2, $3); }
| expr "&&" expr                        { $$ = new BinOp($1, $2, $3); }
| expr comp_operator expr %prec COMPOP  { $$ = new BinOp($1, $2, $3); }
| expr add_sub_operator expr %prec ADDOP    { $$ = new BinOp($1, $2, $3); }
| expr mul_div_operator expr %prec MULTOP   { $$ = new BinOp($1, $2, $3); }
| expr "**" expr                        { $$ = new BinOp($1, $2, $3); }
| unop expr %prec UNOPS                 { $$ = new UnOp($1, $2); }
| "while" expr "do" expr "done"         { $$ = new While($2, $4); }
| "for" T_identifier '=' expr "to" expr "do" expr "done"       { $$ = new For($2, $4, "to", $6, $8); } 
| "for" T_identifier '=' expr "downto" expr "do" expr "done"   { $$ = new For($2, $4, "downto", $6, $8); }  
| "match" expr "with" clause clause_list "end"      { $5->insert($5->begin(), $4); $$ = new Match($2, $5); }
| "dim" T_int_const T_identifier           { Int_Const *dim = new Int_Const($2); $$ = new Dim($3, dim); }
| "dim" T_identifier                       { $$ = new Dim($2); }
| T_identifier expr_2_list                 { $$ = new FunctionCall($1, $2); }
| T_constructor expr_2_list                { $$ = new ConstrCall($1, $2); }
| expr_2                                   { $$ = $1; }            
;

expr_2:
  T_int_const                        { $$ = new Int_Const($1); } 
| T_float_const                      { $$ = new Float_Const($1); }
| T_char_const                       { $$ = new Char_Const($1); }
| T_string_const                   	 { $$ = new String_Const($1); }
| T_identifier                       { $$ = new ConstantCall($1); }
| T_constructor                      { $$ = new ConstrCall($1); }
| "true"                             { $$ = new Bool_Const(true); }
| "false"                            { $$ = new Bool_Const(false); }
| '(' ')'                            { $$ = new Unit(); }
| '!' expr_2                         { $$ = new UnOp('!', $2); }
| T_identifier br_comma_expr_list    { $$ = new ArrayAcc($1, $2); }
| "new" type                         { $$ = new New($2); }
| '(' expr ')'                       { $$ = $2; }
| "begin" expr "end"                 { $$ = $2; }
;



expr_2_list:
  expr_2                        { $$ = new std::vector<Expr *>(); $$->push_back($1); }
| expr_2_list expr_2            { $1->push_back($2); $$ = $1; }
;

unop:
  '+'       { $$ = $1; }
| '-'       { $$ = $1; }
| "+."      { $$ = $1; }
| "-."      { $$ = $1; }
| "not"     { $$ = $1; }
| "delete"  { $$ = $1; }
;

comp_operator:
  '='       { $$ = $1; }
| "<>"      { $$ = $1; }
| '>'       { $$ = $1; }
| '<'       { $$ = $1; }
| "<="      { $$ = $1; }
| ">="      { $$ = $1; }
| "=="      { $$ = $1; }
| "!="      { $$ = $1; }
;

add_sub_operator:
  '+'       { $$ = $1; }
| '-'       { $$ = $1; }
| "+."      { $$ = $1; }
| "-."      { $$ = $1; }
;

mul_div_operator:
  '*'       { $$ = $1; }
| '/'       { $$ = $1; }
| "*."      { $$ = $1; }
| "/."      { $$ = $1; }
| "mod"     { $$ = $1; }
;


clause_list:
  %empty                            { $$ = new std::vector<Clause *>(); }
| clause_list '|' clause    		{ $1->push_back($3); $$ = $1; }
;

clause:
  pattern "->" expr                 { $$ = new Clause($1, $3); }
;

pattern:
  '+' T_int_const                        { $$ = new Pattern_Const(new Int_Const($2));     }
| '-' T_int_const                        { $$ = new Pattern_Const(new Int_Const(-$2));    }
| T_int_const                            { $$ = new Pattern_Const(new Int_Const($1));     }
| "+." T_float_const                     { $$ = new Pattern_Const(new Float_Const($2));   }
| "-." T_float_const                     { $$ = new Pattern_Const(new Float_Const(-$2));  }
| T_float_const                          { $$ = new Pattern_Const(new Float_Const($1));   }
| T_char_const                           { $$ = new Pattern_Const(new Char_Const($1));    }
| "true"                                 { $$ = new Pattern_Const(new Bool_Const(true));  }
| "false"                                { $$ = new Pattern_Const(new Bool_Const(false)); }
| T_identifier                           { $$ = new PatternId($1);        }
| T_constructor                          { $$ = new PatternConstr($1);     }
| '(' pattern ')'                        { $$ = $2;                          }
|  T_constructor pattern_list        	 { $$ = new PatternConstr($1, $2); }
;

pattern_list:
  pattern                           { $$ = new std::vector< Pattern* >(); $$->push_back($1); }
| pattern_list pattern              { $1->push_back($2); $$ = $1; }
;

%%
void yyerror(const char *msg) {
    std::cerr <<  "Error at the line: " << line_nr << ": " <<  msg << '\n';
    exit(1);
}

int main(int argc, char **argv) {
    optionsList.parseOptions(argc, argv);
    int result = yyparse();
    optionsList.executeOptions();
    if (result == 0 && compiledDone.isActivated()) std::cout << "Success\n";
    return result;
}