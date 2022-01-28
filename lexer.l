%option noyywrap 
%option nounput

%{
#include <cstdio>
#include <cstdlib> 
#include <string>
#include "lexer.hpp"
#include "ast.hpp"    
#include "parser.hpp"   
#define T_eof 0

int comment_nr = 0;
int line_nr = 1;
%}

W  	[ \t\n\r]  
D   [0-9]     
Llc [a-z]     
Luc [A-Z]     
L  	[a-zA-Z]
Hex [0-9a-fA-F]
ES	\\([ntr0\\\'\"]|x{Hex}{Hex})
Sym [ !#-&\(-\[\]-~]

%x COMMENT

%%

"and"       { return T_and;     }
"array"     { return T_array;   }
"begin"     { return T_begin;   }
"bool"      { return T_bool;    }
"char"      { return T_char;    }
"delete"    { yylval.op = T_delete; return T_delete;  }
"dim"       { return T_dim;     }
"do"        { return T_do;      }
"done"      { return T_done;    }
"downto"    { return T_downto;  }
"else"      { return T_else;    }
"end"       { return T_end;     }
"false"     { return T_false;   }
"float"     { return T_float;   }
"for"       { return T_for;     }
"if"        { return T_if;      }
"in"        { return T_in;      }
"int"       { return T_int;     }
"let"       { return T_let;     }
"match"     { return T_match;   }
"mod"       { yylval.op = T_mod; return T_mod;     }
"mutable"   { return T_mutable; }
"new"       { return T_new;     }
"not"       { yylval.op = T_not; return T_not;     }
"of"        { return T_of;      }
"rec"       { return T_rec;     }
"ref"       { return T_ref;     }
"then"      { return T_then;    }
"to"        { return T_to;      }
"true"      { return T_true;    }
"type"      { return T_type;    }
"unit"      { return T_unit;    }
"while"     { return T_while;   }
"with"      { return T_with;    }

{Llc}({L}|{D}|_)*    { yylval.id = new std::string(yytext); return T_identifier; }
{Luc}({L}|{D}|_)*    { yylval.id = new std::string(yytext); return T_constructor; }
{D}+                        	{ yylval.num = atoi(yytext); return T_int_const;   }
({D}+\.{D}+([eE][+\-]?{D}+)?)   { yylval.fl = atof(yytext); return T_float_const; }
\'({L}|{D}|{Sym}|{ES})\' 		{ yylval.str = new std::string(yytext); return T_char_const; }
\"({L}|{D}|{Sym}|{ES})*\"   	{ yylval.str = new std::string(yytext); return T_string_const; }

[=\|\+\-\*\/\!\;\<\>\(\)\[\]\,\:]   {  yylval.op = yytext[0]; return yytext[0]; }

"->" { yylval.op = T_mingr; 		return T_mingr;  }
"+." { yylval.op = T_plusdot; 		return T_plusdot;      }
"-." { yylval.op = T_mindot;     	return T_mindot;     }
"*." { yylval.op = T_stardot;      	return T_stardot;      }
"/." { yylval.op = T_sldot;     	return T_sldot;     }
"**" { yylval.op = T_starstar;      return T_starstar;      }
"&&" { yylval.op = T_andand; 		return T_andand; }
"||" { yylval.op = T_barbar;       	return T_barbar;       }
"<>" { yylval.op = T_lgr;  			return T_lgr;  }
"<=" { yylval.op = T_leq;          	return T_leq;          }
">=" { yylval.op = T_greq;          return T_greq;          }
"==" { yylval.op = T_eqeq;        	return T_eqeq;        }
"!=" { yylval.op = T_excleq;     	return T_excleq;     }
":=" { yylval.op = T_coleq;      	return T_coleq;      }

--.*  { /* nothing */ }

"(*"                     { comment_nr++; BEGIN(COMMENT);}
<COMMENT>"(*"            { comment_nr++;}
<COMMENT>"*)"            { comment_nr--; if (comment_nr==0) {BEGIN(INITIAL);} }   
<COMMENT>\n				 { line_nr++   ; } 
<COMMENT>"*"             { /* nothing */ }
<COMMENT>[^*\n]+         { /* nothing */ }
<COMMENT>"("             { /* nothing */ }
<COMMENT><<EOF>>         { yyerror("EOF inside the comment"); }

{W}+    { /* nothing */ } 

. { yyerror("Illegal character"); }

%%


