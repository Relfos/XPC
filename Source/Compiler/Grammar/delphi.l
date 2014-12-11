%s INITIAL XASMCODESEC XFUNCDECL XFUNCPARAMS XFUNCDIRECTS XFUNCEXTERN
%s SPROPSPECS SCLASS


%%

library		Begin Result := KW_LIBRARY; End;
unit		Begin Result := KW_UNIT; End;
program		Begin Result := KW_PROGRAM; End;

uses		Begin Result := KW_USES; End;
exports		Begin Result := KW_EXPORTS; End;

interface		Begin Result := KW_INTERF; End;
implementation	Begin Result := KW_IMPL; End;
finalization	Begin Result := KW_FINALIZ; End;
initialization	Begin Result := KW_INIT; End;

class		Begin pushstate(SCLASS); Result := KW_CLASS; End;
object		Begin pushstate(SCLASS); Result := KW_OBJECT; End;
record		Begin Result := KW_RECORD; End;
packed		Begin Result := KW_PACKED; End;		
of			Begin Result := KW_OF; End;			
out			Begin Result := KW_OUT; End;			
absolute	Begin Result := KW_ABSOLUTE; End;		
property	Begin pushstate(SPROPSPECS); Result := KW_PROPERTY; End;
inherited	Begin Result := KW_INHERITED; End;	


function	Begin pushstate(XFUNCDECL); Result := KW_FUNCTION; End;
procedure	Begin pushstate(XFUNCDECL); Result := KW_PROCEDURE; End;
constructor	Begin pushstate(XFUNCDECL); Result := KW_CONSTRUCTOR; End;
destructor	Begin pushstate(XFUNCDECL); Result := KW_DESTRUCTOR; End;

const		Begin Result := KW_CONST; End;
var			Begin Result := KW_VAR; End;
threadvar	Begin Result := KW_THRVAR; End;
type		Begin Result := KW_TYPE; End;
resourcestring	Begin Result := TYPE_RSCSTR; End;

Begin		Begin Result := KW_Begin; End;
with		Begin Result := KW_WITH; End; 	
do			Begin Result := KW_DO; End;

for			Begin Result := KW_FOR; End;
to			Begin Result := KW_TO; End;
downto		Begin Result := KW_DOWNTO; End;
repeat		Begin Result := KW_REPEAT; End;
until		Begin Result := KW_UNTIL; End;
while		Begin Result := KW_WHILE; End;
break		Begin Result := KW_BREAK; End;
continue	Begin Result := KW_CONTINUE; End;

if			Begin Result := KW_IF; End;
then		Begin Result := KW_THEN; End;
else		Begin Result := KW_ELSE; End;
case		Begin Result := KW_CASE; End;
goto		Begin Result := KW_GOTO; End;
label		Begin Result := KW_LABEL; End;

raise		Begin Result := KW_RAISE; End;
at			Begin Result := KW_AT; End;	
try			Begin Result := KW_TRY; End;
except		Begin Result := KW_EXCEPT; End;
finally		Begin Result := KW_FINALLY; End;
on			Begin Result := KW_ON; End;


<XFUNCDECL>ID	Begin 
							Result := ProcessIdentifier(yytext); 
						End;
<XFUNCDECL>"."		Begin 
						Result := KW_DOT; 
					End;
	
<XFUNCDECL>"("		Begin
						switchcallback(XFUNCPARAMS, INITIAL);
						Result := LPAR;
					End;
<XFUNCDECL>";"		Begin
						switchstate(XFUNCDIRECTS);
						Result := SCOL;
					End;
<XFUNCDECL>":"		Begin
						switchcallback(XFUNCDIRECTS, INITIAL);
						Result := COLON;
					End;
<XFUNCDECL>[ \t\n\r]+	Begin
						End;
<XFUNCDECL>. 			Begin
							popstate();
							yypushback(1);
						End;
						
<XFUNCPARAMS>":"	Begin
						switchcallback(XFUNCDIRECTS, INITIAL);
						Result := COLON;
					End;
	
<XFUNCPARAMS>";"	Begin
						switchstate(XFUNCDIRECTS);
						Result := SCOL;
					End;
				
<XFUNCPARAMS>[ \t\n\r]+	Begin
						End;
<XFUNCPARAMS>. 			Begin
							popstate();
							yypushback(1);
						End;
	
<XFUNCDIRECTS>cdecl		Begin Result := KW_CDECL; End;
<XFUNCDIRECTS>pascal	Begin Result := KW_PASCAL; End;
<XFUNCDIRECTS>mwpascal	Begin Result := KW_PASCAL; End;
<XFUNCDIRECTS>register	Begin Result := KW_REGISTER; End;
<XFUNCDIRECTS>safecall	Begin Result := KW_SAFECALL; End;
<XFUNCDIRECTS>stdcall	Begin Result := KW_STDCALL; End;

<XFUNCDIRECTS>abstract	Begin Result := KW_ABSTRACT; End;
<XFUNCDIRECTS>assembler	Begin Result := KW_ASSEMBLER; End;
<XFUNCDIRECTS>dynamic		Begin Result := KW_DYNAMIC; End;
<XFUNCDIRECTS>export		Begin Result := KW_EXPORT; End;
<XFUNCDIRECTS>forward		Begin Result := KW_FORWARD; End;
<XFUNCDIRECTS>inline		Begin Result := KW_INLINE; End;
<XFUNCDIRECTS>override	Begin Result := KW_OVERRIDE; End;
<XFUNCDIRECTS>overload	Begin Result := KW_OVERLOAD; End;
<XFUNCDIRECTS>reintroduce	Begin Result := KW_REINTRODUCE; End;
<XFUNCDIRECTS>virtual		Begin Result := KW_VIRTUAL; End;
<XFUNCDIRECTS>varargs		Begin Result := KW_VARARGS; End;
	
<XFUNCDIRECTS>far			Begin Result := KW_FAR; End;
<XFUNCDIRECTS>near		Begin Result := KW_NEAR; End;
<XFUNCDIRECTS>resident	Begin Result := KW_RESIDENT; End;
		
<XFUNCDIRECTS>external	Begin pushstate(XFUNCEXTERN); Result := KW_EXTERNAL; End;

<XFUNCDIRECTS>";"			Begin Result := SCOL; End;
<XFUNCDIRECTS>[ \t\n\r]+	Begin End;

<XFUNCDIRECTS>"="			Begin popstate(); Result := KW_EQ; End;

	
<XFUNCDIRECTS>ID	Begin 
						popstate(); 
						yypushback(yylength()); 
						End;
			
<XFUNCDIRECTS>. Begin 
					popstate();	
					yypushback(1); 
				End;

<XFUNCEXTERN>name 		Begin Result := KW_NAME; End;
<XFUNCEXTERN>'[^']*'		Begin yylval.yyStringObject := StringObject.Create(Copy(yytext, 2, yylength()-2)); Result := CONST_STR; End;
<XFUNCEXTERN>"."			Begin Result := KW_DOT; End;
<XFUNCEXTERN>ID				Begin Result := ProcessIdentifier(yytext); End;
	
<XFUNCEXTERN>";"			Begin popstate(); Result := SCOL; End;
<XFUNCEXTERN>[ \t\n\r]+	Begin End;
<XFUNCEXTERN>.			Begin yyerror('Invalid char in external args: ' + yytext); End;	

<SPROPSPECS>default		Begin Result := KW_DEFAULT; End;
<SPROPSPECS>index		Begin Result := KW_INDEX; End;
<SPROPSPECS>implements	Begin Result := KW_IMPLEMENTS; End;
<SPROPSPECS>nodefault	Begin Result := KW_NODEFAULT; End;
<SPROPSPECS>read		Begin Result := KW_READ; End;
<SPROPSPECS>stored		Begin Result := KW_STORED; End;
<SPROPSPECS>write		Begin Result := KW_WRITE; End;

<SPROPSPECS>";" 	Begin
						popstate();
						Result := SCOL;
					End;
							
<SPROPSPECS>"["		Begin	
						pushstate(INITIAL); 
						Result := LBRAC; 
					End;

<SCLASS>protected	Begin Result := KW_PROTECTED;	End;
<SCLASS>public		Begin Result := KW_PUBLIC;		End;
<SCLASS>published	Begin Result := KW_PUBLISHED;	End;
<SCLASS>private		Begin Result := KW_PRIVATE;	End;

<SCLASS>end			Begin
						popstate();
						Result := KW_End;
					End;

end				Begin Result := KW_End; End;

	
byte		Begin Result := TYPE_BYTE; End;
shortint	Begin Result := TYPE_SHORTINT; End;
word		Begin Result := TYPE_WORD; End;
smallint	Begin Result := TYPE_SMALLINT; End;
cardinal	Begin Result := TYPE_CARDINAL; End;
integer		Begin Result := TYPE_INT; End;
longword	Begin Result := TYPE_LONGWORD; End;
longint		Begin Result := TYPE_LONGINT; End;
int64		Begin Result := TYPE_INT64; End;
uint64		Begin Result := TYPE_UINT64; End;
comp		Begin Result := TYPE_COMP; End;

single		Begin Result := TYPE_FLOAT; End;
real48		Begin Result := TYPE_REAL48; End;
extended	Begin Result := TYPE_EXTENDED; End;
double		Begin Result := TYPE_DOUBLE; End;
real		Begin Result := TYPE_DOUBLE; End;
currency	Begin Result := TYPE_CURR; End;

char		Begin Result := TYPE_CHAR; End;
pchar		Begin Result := TYPE_PCHAR; End;
ansichar	Begin Result := TYPE_PCHAR; End;
widechar	Begin Result := TYPE_WIDECHAR; End;
shortstring	Begin Result := TYPE_SHORTSTR; End;
widestring	Begin Result := TYPE_WIDESTR; End;
ansistring	Begin Result := TYPE_STR; End;
		 
boolean		Begin Result := TYPE_BOOL; End;
olevariant	Begin Result := TYPE_OLEVAR; End;
variant		Begin Result := TYPE_VAR; End;

string		Begin Result := TYPE_STR; End;
array		Begin Result := TYPE_ARRAY; End;
pointer		Begin Result := TYPE_PTR; End;
file		Begin Result := TYPE_FILE; End;
set			Begin Result := TYPE_SET; End;
		

asm					Begin
						pushstate(XASMCODESEC);
						Result := KW_ASM;
					End;
<XASMCODESEC>[ \t\r]+	Begin 
						End;
					
<XASMCODESEC>end	Begin
						popstate();
						Result := KW_End;
					End;
	
<XASMCODESEC>ID		Begin
					End;

<XASMCODESEC>.|(\r|\n|\r\n)	Begin
							End;

	
[0-9]+				Begin  yylval.yyInteger := StringToInt(yytext); Result:= CONST_INT; End;
[$][0-9a-f]*				Begin yylval.yyInteger := HexStrToInt(Copy(yytext, 2, MaxInt)); Result:= CONST_INT; End;
nil					Begin Result := CONST_NIL; End;
[0-9]+\.[0-9]+		Begin yylval.yyDouble := StringToFloat(yytext); Result:= CONST_REAL; End;
[0-9]+e[+-]?[0-9]+	Begin yylval.yyDouble := StringToFloat(yytext); Result:= CONST_REAL; End;
[0-9]+\.[0-9]+e[+-]?[0-9]+	Begin yylval.yyDouble := StringToFloat(yytext); Result:=  CONST_REAL; End;
	
	
'.'					Begin yylval.yyAnsiChar := yytext[1]; Result:= CONST_CHAR; End;
					
\#[0-9]+			Begin yylval.yyAnsiChar := AnsiChar(StringToInt(Copy(yytext, 2, MaxInt))); Result:= CONST_CHAR; End;
\#[$][0-9a-f]*+			Begin yylval.yyAnsiChar := AnsiChar(HexStrToInt(Copy(yytext, 2, MaxInt))); Result:= CONST_CHAR; End;
					
'[^']*'				Begin yylval.yyStringObject := StringObject.Create(Copy(yytext, 2, yylength-2)); Result:= CONST_STR; End;

true				Begin yylval.yyBoolean := true;  Result:= CONST_BOOL; End;
false				Begin yylval.yyBoolean := false; Result:= CONST_BOOL; End;


	{ Operators	}
	
	{ separators }
":"			Begin Result := COLON; End;
","			Begin Result := COMMA; End;
"["			Begin Result := LBRAC; End;
"("			Begin Result := LPAR ; End;

"]"			Begin if (yylaststate() = SPROPSPECS  ) Then popstate(); Result := RBRAC; End;
";"			Begin if (yylaststate() = XFUNCDIRECTS) Then popstate(); Result := SCOL; End;
")"			Begin if (yylaststate() = XFUNCPARAMS ) Then popstate(); Result := RPAR; End;

	{ expr: general }
":="		Begin Result := KW_ASSIGN; End;
".."		Begin Result := KW_RANGE; End;
as			Begin Result := KW_AS; End;	

	{ expr: unary pointer access }
"."			Begin Result := KW_DOT; End;
"@"			Begin Result := KW_ADDR; End;
"^"			Begin Result := KW_DEREF; End;

	{ expr: numeric operators }
"-"			Begin Result := KW_SUB; End;
"+"			Begin Result := KW_SUM; End;
"/"			Begin Result := KW_DIV; End;
"*"			Begin Result := KW_MUL; End;
div			Begin Result := KW_QUOT; End;
mod			Begin Result := KW_MOD; End;

	{ expr: logical operators }
and			Begin Result := KW_AND; End;
or			Begin Result := KW_OR;	End;
xor			Begin Result := KW_XOR; End;
shl			Begin Result := KW_SHL; End;
shr			Begin Result := KW_SHR; End;
not			Begin Result := KW_NOT; End;

	{ expr: comparison operators }
"<"			Begin Result := KW_LT; End;
">"			Begin Result := KW_GT; End;
">="		Begin Result := KW_GE; End;
"<="		Begin Result := KW_LE; End;
"="			Begin Result := KW_EQ; End;
"<>"		Begin Result := KW_NE; End;
is			Begin Result := KW_IS; End;	
in			Begin Result := KW_IN; End;	




	{************************************************************}

ID			Begin Result := ProcessIdentifier(yytext); End;

[ \t\n]+	Begin
			End;

.			Begin
				yyerror('Unknown char: ' + yytext ); 
			End;



	{**********************************************************}
	{ Obscure/Non-portable/Deprecated keywords: not supported	}
	
	{ file warnings }
	{
	platform	Begin Result := KW_PLATFORM; End;
	deprecated	Begin Result := KW_DEPRECATED; End;
	}

	{ Embarcadero Delphi directives - ignored
	delayed experimental final
	helper operator reference
	sealed static strict unsafe
	}

	{		
	winapi		Begin Result := KW_WINAPI; End;
	message		Begin Result := KW_MESSAGE; End;
	dispinterface	Begin Result := KW_DISPINTERF; End;
	dispid		Begin Result := KW_DISPID; End;
	automated	Begin Result := KW_AUTOMATED; End;	// visibilidade
		// properties modifiers for dispinterface
	writeonly	Begin Result := KW_WRITEONLY; End;
	readonly	Begin Result := KW_READONLY; End;
	}

%%
