D [0-9]
E [Ee][+-]?{D}+ 
F [a-zA-Z][a-zA-Z0-9]*
%{
#include <math.h>
#include "mcalc.tab.h"

%}


%%
{D}*[.]?{D}+{E}?	{ yylval.val = yytext; return NUM; }

[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN; }
[)]		{ return CLOSE; }
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
if      { return IF;  }
then    { return THEN; }
ifnot   { return IFNOT; }
let     { return LET; }
[=]     { return EQUALS; }
(<-)    { return RECEIVE; }
do      { return DO; }
and     { return AND; }
function  { return FUNCTION; }
{F}       { yylval.val = yytext; return FUNC; } 

%%

/* Local Variables: */
/* mode: c */
/* End: */
