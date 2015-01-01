ID [_a-zA-Z][0-9a-zA-Z_]*

%s INITIAL XASMCODESEC XFUNCDECL XFUNCPARAMS XFUNCDIRECTS XFUNCEXTERN
%s SPROPSPECS SCLASS


%start

%%

library		Begin Return(KW_LIBRARY); End;
unit		Begin Return(KW_UNIT); End;
program		Begin Return(KW_PROGRAM); End;

uses		Begin Return(KW_USES); End;
exports		Begin Return(KW_EXPORTS); End;

interface		Begin Return(KW_INTERF); End;
implementation	Begin Return(KW_IMPL); End;
finalization	Begin Return(KW_FINALIZ); End;
initialization	Begin Return(KW_INIT); End;

class		Begin pushstate(SCLASS); Return(KW_CLASS); End;
object		Begin pushstate(SCLASS); Return(KW_OBJECT); End;
record		Begin Return(KW_RECORD); End;
packed		Begin Return(KW_PACKED); End;		
of			Begin Return(KW_OF); End;			
out			Begin Return(KW_OUT); End;			
absolute	Begin Return(KW_ABSOLUTE); End;		
property	Begin pushstate(SPROPSPECS); Return(KW_PROPERTY); End;
inherited	Begin Return(KW_INHERITED); End;	


function	Begin pushstate(XFUNCDECL); Return(KW_FUNCTION); End;
procedure	Begin pushstate(XFUNCDECL); Return(KW_PROCEDURE); End;
constructor	Begin pushstate(XFUNCDECL); Return(KW_CONSTRUCTOR); End;
destructor	Begin pushstate(XFUNCDECL); Return(KW_DESTRUCTOR); End;

const		Begin Return(KW_CONST); End;
var			Begin Return(KW_VAR); End;
threadvar	Begin Return(KW_THRVAR); End;
type		Begin Return(KW_TYPE); End;
resourcestring	Begin Return(TYPE_RSCSTR); End;

begin		Begin Return(KW_Begin); End;
with		Begin Return(KW_WITH); End; 	
do			Begin Return(KW_DO); End;

for			Begin Return(KW_FOR); End;
to			Begin Return(KW_TO); End;
downto		Begin Return(KW_DOWNTO); End;
repeat		Begin Return(KW_REPEAT); End;
until		Begin Return(KW_UNTIL); End;
while		Begin Return(KW_WHILE); End;
break		Begin Return(KW_BREAK); End;
continue	Begin Return(KW_CONTINUE); End;

if			Begin Return(KW_IF); End;
then		Begin Return(KW_THEN); End;
else		Begin Return(KW_ELSE); End;
case		Begin Return(KW_CASE); End;
goto		Begin Return(KW_GOTO); End;
label		Begin Return(KW_LABEL); End;

raise		Begin Return(KW_RAISE); End;
at			Begin Return(KW_AT); End;	
try			Begin Return(KW_TRY); End;
except		Begin Return(KW_EXCEPT); End;
finally		Begin Return(KW_FINALLY); End;
on			Begin Return(KW_ON); End;

<XFUNCDECL>{ID}	    Begin 
                        Return(ProcessIdentifier(yytext)); 
						End;
<XFUNCDECL>"."		Begin 
						Return(KW_DOT); 
					End;
	
<XFUNCDECL>"("		Begin
						switchcallback(XFUNCPARAMS, INITIAL);
						Return(LPAR);
					End;
<XFUNCDECL>";"		Begin
						switchstate(XFUNCDIRECTS);
						Return(SCOL);
					End;
<XFUNCDECL>":"		Begin
						switchcallback(XFUNCDIRECTS, INITIAL);
						Return(COLON);
					End;
<XFUNCDECL>[ \t\n\r]+	Begin
						End;
<XFUNCDECL>. 			Begin
							popstate();
							yypushback(1);
						End;
						
<XFUNCPARAMS>":"	Begin
						switchcallback(XFUNCDIRECTS, INITIAL);
						Return(COLON);
					End;
	
<XFUNCPARAMS>";"	Begin
						switchstate(XFUNCDIRECTS);
						Return(SCOL);
					End;
				
<XFUNCPARAMS>[ \t\n\r]+	Begin
						End;
<XFUNCPARAMS>. 			Begin
							popstate();
							yypushback(1);
						End;
	
<XFUNCDIRECTS>cdecl		Begin Return(KW_CDECL); End;
<XFUNCDIRECTS>pascal	Begin Return(KW_PASCAL); End;
<XFUNCDIRECTS>mwpascal	Begin Return(KW_PASCAL); End;
<XFUNCDIRECTS>register	Begin Return(KW_REGISTER); End;
<XFUNCDIRECTS>safecall	Begin Return(KW_SAFECALL); End;
<XFUNCDIRECTS>stdcall	Begin Return(KW_STDCALL); End;

<XFUNCDIRECTS>abstract	Begin Return(KW_ABSTRACT); End;
<XFUNCDIRECTS>assembler	Begin Return(KW_ASSEMBLER); End;
<XFUNCDIRECTS>dynamic		Begin Return(KW_DYNAMIC); End;
<XFUNCDIRECTS>export		Begin Return(KW_EXPORT); End;
<XFUNCDIRECTS>forward		Begin Return(KW_FORWARD); End;
<XFUNCDIRECTS>inline		Begin Return(KW_INLINE); End;
<XFUNCDIRECTS>override	Begin Return(KW_OVERRIDE); End;
<XFUNCDIRECTS>overload	Begin Return(KW_OVERLOAD); End;
<XFUNCDIRECTS>reintroduce	Begin Return(KW_REINTRODUCE); End;
<XFUNCDIRECTS>virtual		Begin Return(KW_VIRTUAL); End;
<XFUNCDIRECTS>varargs		Begin Return(KW_VARARGS); End;
	
<XFUNCDIRECTS>far			Begin Return(KW_FAR); End;
<XFUNCDIRECTS>near		Begin Return(KW_NEAR); End;
<XFUNCDIRECTS>resident	Begin Return(KW_RESIDENT); End;
		
<XFUNCDIRECTS>external	Begin pushstate(XFUNCEXTERN); Return(KW_EXTERNAL); End;

<XFUNCDIRECTS>";"			Begin Return(SCOL); End;
<XFUNCDIRECTS>[ \t\n\r]+	Begin End;

<XFUNCDIRECTS>"="			Begin popstate(); Return(KW_EQ); End;

	
<XFUNCDIRECTS>{ID}	Begin 
                                popstate(); 
                                yypushback(yylength()); 
                            End;
			
<XFUNCDIRECTS>. Begin 
					popstate();	
					yypushback(1); 
				End;

<XFUNCEXTERN>name 		Begin Return(KW_NAME); End;
<XFUNCEXTERN>'[^']*'		Begin yylval.yyStringObject := StringObject.Create(Copy(yytext, 2, yylength()-2)); Return(CONST_STR); End;
<XFUNCEXTERN>"."			Begin Return(KW_DOT); End;
<XFUNCEXTERN>{ID}		Begin Return(ProcessIdentifier(yytext)); End;
	
<XFUNCEXTERN>";"			Begin popstate(); Return(SCOL); End;
<XFUNCEXTERN>[ \t\n\r]+	Begin End;
<XFUNCEXTERN>.			Begin yyerror('Invalid char in external args: ' + yytext); End;	

<SPROPSPECS>default		Begin Return(KW_DEFAULT); End;
<SPROPSPECS>index		Begin Return(KW_INDEX); End;
<SPROPSPECS>implements	Begin Return(KW_IMPLEMENTS); End;
<SPROPSPECS>nodefault	Begin Return(KW_NODEFAULT); End;
<SPROPSPECS>read		Begin Return(KW_READ); End;
<SPROPSPECS>stored		Begin Return(KW_STORED); End;
<SPROPSPECS>write		Begin Return(KW_WRITE); End;

<SPROPSPECS>";" 	Begin
						popstate();
						Return(SCOL);
					End;
							
<SPROPSPECS>"["		Begin	
						pushstate(INITIAL); 
						Return(LBRAC); 
					End;

<SCLASS>protected	Begin Return(KW_PROTECTED);	End;
<SCLASS>public		Begin Return(KW_PUBLIC);		End;
<SCLASS>published	Begin Return(KW_PUBLISHED);	End;
<SCLASS>private		Begin Return(KW_PRIVATE);	End;

<SCLASS>end			Begin
						popstate();
						Return(KW_END);
					End;

end				Begin Return(KW_END); End;

string		Begin Return(TYPE_STR); End;
array		Begin Return(TYPE_ARRAY); End;
pointer		Begin Return(TYPE_PTR); End;
file		Begin Return(TYPE_FILE); End;
set			Begin Return(TYPE_SET); End;
		

asm					Begin
						pushstate(XASMCODESEC);
						Return(KW_ASM);
					End;
<XASMCODESEC>[ \t\r]+	Begin 
						End;
					
<XASMCODESEC>end	Begin
						popstate();
						Return(KW_END);
					End;
	
<XASMCODESEC>{ID}	    Begin
                            End;

<XASMCODESEC>.|(\r|\n|\r\n)	Begin
							End;

	
[0-9]+				Begin  yylval.yyInteger := StringToInt(yytext); Return(CONST_INT); End;
[$][0-9a-f]*				Begin yylval.yyInteger := HexStrToInt(Copy(yytext, 2, MaxInt)); Return(CONST_INT); End;
nil					Begin Return(CONST_NIL); End;
[0-9]+\.[0-9]+		Begin yylval.yyDouble := StringToFloat(yytext); Return(CONST_REAL); End;
[0-9]+e[+-]?[0-9]+	Begin yylval.yyDouble := StringToFloat(yytext); Return(CONST_REAL); End;
[0-9]+\.[0-9]+e[+-]?[0-9]+	Begin yylval.yyDouble := StringToFloat(yytext); Return(CONST_REAL); End;
	
	
'.'					Begin yylval.yyAnsiChar := yytext[1]; Return(CONST_CHAR); End;
					
\#[0-9]+			Begin yylval.yyAnsiChar := AnsiChar(StringToInt(Copy(yytext, 2, MaxInt))); Return(CONST_CHAR); End;
\#[$][0-9a-f]*+			Begin yylval.yyAnsiChar := AnsiChar(HexStrToInt(Copy(yytext, 2, MaxInt))); Return(CONST_CHAR); End;
					
'[^']*'				Begin yylval.yyStringObject := StringObject.Create(Copy(yytext, 2, yylength-2)); Return(CONST_STR); End;

true				Begin yylval.yyBoolean := true;  Return(CONST_BOOL); End;
false				Begin yylval.yyBoolean := false; Return(CONST_BOOL); End;

":"			Begin Return(COLON); End;
","			Begin Return(COMMA); End;
"["			Begin Return(LBRAC); End;
"("			Begin Return(LPAR ); End;

"]"			Begin if (yylaststate() = SPROPSPECS  ) Then popstate(); Return(RBRAC); End;
";"			Begin if (yylaststate() = XFUNCDIRECTS) Then popstate(); Return(SCOL); End;
")"			Begin if (yylaststate() = XFUNCPARAMS ) Then popstate(); Return(RPAR); End;

":="		Begin Return(KW_ASSIGN); End;
".."		Begin Return(KW_RANGE); End;
as			Begin Return(KW_AS); End;	

"."			Begin Return(KW_DOT); End;
"@"			Begin Return(KW_ADDR); End;
"^"			Begin Return(KW_DEREF); End;

"-"			Begin Return(KW_SUB); End;
"+"			Begin Return(KW_SUM); End;
"/"			Begin Return(KW_DIV); End;
"*"			Begin Return(KW_MUL); End;
div			Begin Return(KW_QUOT); End;
mod			Begin Return(KW_MOD); End;

and			Begin Return(KW_AND); End;
or			Begin Return(KW_OR);	End;
xor			Begin Return(KW_XOR); End;
shl			Begin Return(KW_SHL); End;
shr			Begin Return(KW_SHR); End;
not			Begin Return(KW_NOT); End;

"<"			Begin Return(KW_LT); End;
">"			Begin Return(KW_GT); End;
">="		Begin Return(KW_GE); End;
"<="		Begin Return(KW_LE); End;
"="			Begin Return(KW_EQ); End;
"<>"		Begin Return(KW_NE); End;
is			Begin Return(KW_IS); End;	
in			Begin Return(KW_IN); End;	

{ID}    Begin Return(ProcessIdentifier(yytext)); End;

[ \t\n]+	Begin
			End;

.		    Begin
				yyerror('Unknown char: ' + yytext ); 
			End;
