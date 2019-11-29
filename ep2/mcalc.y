/* Compilador da nossa linguagem pep */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char *oper(char op, char *l, char *r) {
	char *res = malloc(strlen(l)+strlen(r)+6);
	sprintf(res, "(%c %s %s)", op, l, r);
	return res;
}
char *dup(char *orig) {
	char *res = malloc(strlen(orig)+1);
	strcpy(res,orig);
	return res;
}

char *seq(char *exp1, char *exp2) {
	char *res = malloc(strlen(exp1)+strlen(exp2)+8);
	sprintf(res, "(seq %s %s)", exp1, exp2);
	return res;
}

char *se(char *cond, char *then, char *ifnot) {
	char *res = malloc(strlen(cond)+strlen(then)+strlen(ifnot)+10);
	sprintf(res, "(if %s %s %s)", cond, then, ifnot);
	return res;
}


char *chama(char *nome, char *arg) {
	char *res = malloc(strlen(nome) + strlen(arg) + 8);
	sprintf(res, "(chama %s %s)", nome, arg);
	return res;
}
char *let( char *simbolo, char *valor, char *faca) {
	char *res = malloc(strlen(simbolo) + strlen(valor) + strlen(faca) + 9);
	sprintf(res, "(def %s %s %s)", simbolo, valor, faca);
	return res;
}
char *atribui( char *simbolo, char *valor) {
	char *res = malloc(strlen(simbolo) + strlen(valor) + 6);
	sprintf(res, "(:= %s %s)", simbolo, valor);
	return res;
}


char *func(char *nome, char *arg, char *corpo, char *faca) {
	char *res = malloc(2*strlen(nome) + strlen(arg) + strlen(corpo) + strlen(faca) + 40);
	sprintf(res, "(def %s 1 (seq (:= %s (func %s %s)) %s))", nome, nome, arg, corpo, faca);
	return res;
}

int yylex();
void yyerror(char *);
%}

%union {
	char *val;
}


%left ADD SUB
%left MUL DIV
%left NEG
%left FUNC


%token	<val> NUM
%token  <val> FUNC
%token  ADD SUB MUL OPEN CLOSE IF THEN IFNOT LET EQUALS DO RECEIVE AND FUNCTION
%type	<val> exp
%type	<val> func

/* Gramatica */
%%

input: 		
		| 		exp     { puts($1);}
		|		func	{ puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 				NUM 		{ $$ = dup($1); }
		| 		exp ADD exp					{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp					{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp					{ $$ = oper('*', $1, $3);}
		|		exp DIV exp 				{ $$ = oper('/', $1, $3);}
		| 		SUB exp %prec NEG 			{ $$ = oper('~', $2, "");}
		|		IF exp THEN exp 			{ $$ = se($2, $4, "");}
		|		IF exp THEN exp IFNOT exp 	{ $$ = se($2, $4, $6);}
		| 		OPEN exp CLOSE 				{ $$ = dup($2);}
		|		LET func EQUALS exp DO exp  { $$ = let($2, $4, $6); }
		|		func RECEIVE exp 			{ $$ = atribui($1, $3); }
		|		OPEN exp AND exp CLOSE		{ $$ = seq($2, $4);}
		|		LET func FUNCTION OPEN exp CLOSE exp DO exp { $$ = func($2, $5, $7, $9); }
		|		func 						{ $$ = dup($1);}
;

func:				FUNC { $$ = dup($1);}
		|		func OPEN exp CLOSE { $$ = chama($1, $3); }
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
