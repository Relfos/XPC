/*** 
 *** C# parser/scanner
 *** Copyright 2002 James Power, NUI Maynooth, Ireland <james.power@may.ie>
 *** This version: 19 Feb 2002
 ***
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY <copyright holder> ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ***/
%{
#include <string.h>
#include "csharp.tab.h"

  void lexical_error(const char *);
  static int token_for(const char *);
  static int is_allowed_char(const char );
%}

%option noyywrap
%option yylineno
%option stack

single_line_comment    "//".*

white_space            [ \t\n\r]

preprocessor           ^[ \t]*#.*

dec_digit              [0-9]
hex_digit              [0-9A-Fa-f]
int_suffix             [UuLl]|[Uu][Ll]|[Ll][Uu]
dec_literal            {dec_digit}+{int_suffix}?
hex_literal            0[xX]{hex_digit}+{int_suffix}?
integer_literal        {dec_literal}|{hex_literal}

real_suffix            [FfDdMm]
sign                   [+\-]
exponent_part          [eE]{sign}?{dec_digit}+
whole_real1            {dec_digit}+{exponent_part}{real_suffix}?
whole_real2            {dec_digit}+{real_suffix}
part_real              {dec_digit}*\.{dec_digit}+{exponent_part}?{real_suffix}?
real_literal           {whole_real1}|{whole_real2}|{part_real}

single_char            [^\\\']
simple_esc_seq         \\[\'\"\\0abfnrtv]
uni_esc_seq1           \\u{hex_digit}{4}
uni_esc_seq2           \\U{hex_digit}{8}
uni_esc_seq            {uni_esc_seq1}|{uni_esc_seq2}
hex_esc_seq            \\x{hex_digit}{1,4}
character              {single_char}|{simple_esc_seq}|{hex_esc_seq}|{uni_esc_seq}
character_literal      \'{character}\'


single_string_char     [^\\\"]
reg_string_char        {single_string_char}|{simple_esc_seq}|{hex_esc_seq}|{uni_esc_seq}
regular_string         \"{reg_string_char}*\"
single_verbatim_char   [^\"]
quote_esc_seq          \"\"
verb_string_char       {single_verbatim_char}|{quote_esc_seq}
verbatim_string        @\"{verb_string_char}*\"
string_literal         {regular_string}|{verbatim_string}


letter_char            [A-Za-z]
ident_char             {dec_digit}|{letter_char}|"_"|"@"
identifier             ({letter_char}|"_"){ident_char}*
at_identifier          \@{identifier}


rank_specifier         "["{white_space}*(","{white_space}*)*"]"


%x IN_COMMENT

%s IN_ATTRIB IN_ACCESSOR IN_GETSET

%%

{preprocessor}        { ; /* ignore */ }
{white_space}         { ; /* ignore */ }

                      /***** Comments *****/
"/*"                  { yy_push_state(IN_COMMENT); }
<IN_COMMENT>.         { ; /* ignore */ }
<IN_COMMENT>\n        { ; /* ignore */ }
<IN_COMMENT>"*/"      { yy_pop_state(); }

{single_line_comment} { ; /* ignore */ }

                      /***** Literals *****/
{integer_literal}     { return INTEGER_LITERAL; }
{real_literal}        { return REAL_LITERAL; }
{character_literal}   { return CHARACTER_LITERAL; }
{string_literal}      { return STRING_LITERAL; }

                      /*** Punctuation and Single-Character Operators ***/
","   { return COMMA; }
"["   { return LEFT_BRACKET; }
"]"   { return RIGHT_BRACKET; }

{rank_specifier}     { return RANK_SPECIFIER; }

                      /*** Multi-Character Operators ***/
"+="  { return PLUSEQ; }
"-="  { return MINUSEQ; }
"*="  { return STAREQ; }
"/="  { return DIVEQ; }
"%="  { return MODEQ; }
"^="  { return XOREQ; }
"&="  { return ANDEQ; }
"|="  { return OREQ; }
"<<"  { return LTLT; }
">>"  { return GTGT; }
">>="  { return GTGTEQ; }
"<<="  { return LTLTEQ; }
"=="  { return EQEQ; }
"!="  { return NOTEQ; }
"<="  { return LEQ; }
">="  { return GEQ; }
"&&"  { return ANDAND; }
"||"  { return OROR; }
"++"  { return PLUSPLUS; }
"--"  { return MINUSMINUS; }
"->"  { return ARROW; }

      
                      /*** Those context-sensitive "keywords" ***/

<IN_ATTRIB>"assembly"    { return ASSEMBLY; }
<IN_ATTRIB>"field"       { return FIELD; }
<IN_ATTRIB>"method"      { return METHOD; }
<IN_ATTRIB>"module"      { return MODULE; }
<IN_ATTRIB>"param"       { return PARAM; }
<IN_ATTRIB>"property"    { return PROPERTY; }
<IN_ATTRIB>"type"        { return TYPE; }

<IN_ACCESSOR>"add"       { return ADD; }
<IN_ACCESSOR>"remove"    { return REMOVE; }

<IN_GETSET>"get"         { return GET; }
<IN_GETSET>"set"         { return SET; }

{identifier}          { return token_for(yytext); }
{at_identifier}       { return IDENTIFIER; }


.     { 
        if (is_allowed_char(yytext[0])) return yytext[0];
        else lexical_error("invalid token"); 
      }
%%


static struct name_value {
  char *name;
  int value;
} name_value;
static struct name_value keywords [] = { 
 /* This list must remain sorted!!! */
    {"abstract", ABSTRACT},
    {"as", AS},
    {"base", BASE},
    {"bool", BOOL},
    {"break", BREAK},
    {"byte", BYTE},
    {"case", CASE},
    {"catch", CATCH},
    {"char", CHAR},
    {"checked", CHECKED},
    {"class", CLASS},
    {"const", CONST},
    {"continue", CONTINUE},
    {"decimal", DECIMAL},
    {"default", DEFAULT},
    {"delegate", DELEGATE},
    {"do", DO},
    {"double", DOUBLE},
    {"else", ELSE},
    {"enum", ENUM},
    {"event", EVENT},
    {"explicit", EXPLICIT},
    {"extern", EXTERN},
    {"false", FALSE},
    {"finally", FINALLY},
    {"fixed", FIXED},
    {"float", FLOAT},
    {"for", FOR},
    {"foreach", FOREACH},
    {"goto", GOTO},
    {"if", IF},
    {"implicit", IMPLICIT},
    {"in", IN},
    {"int", INT},
    {"interface", INTERFACE},
    {"internal", INTERNAL},
    {"is", IS},
    {"lock", LOCK},
    {"long", LONG},
    {"namespace", NAMESPACE},
    {"new", NEW},
    {"null", NULL_LITERAL},
    {"object", OBJECT},
    {"operator", OPERATOR},
    {"out", OUT},
    {"override", OVERRIDE},
    {"params", PARAMS},
    {"private", PRIVATE},
    {"protected", PROTECTED},
    {"public", PUBLIC},
    {"readonly", READONLY},
    {"ref", REF},
    {"return", RETURN},
    {"sbyte", SBYTE},
    {"sealed", SEALED},
    {"short", SHORT},
    {"sizeof", SIZEOF},
    {"stackalloc", STACKALLOC},
    {"static", STATIC},
    {"string", STRING},
    {"struct", STRUCT},
    {"switch", SWITCH},
    {"this", THIS},
    {"throw", THROW},
    {"true", TRUE},
    {"try", TRY},
    {"typeof", TYPEOF},
    {"uint", UINT},
    {"ulong", ULONG},
    {"unchecked", UNCHECKED},
    {"unsafe", UNSAFE},
    {"ushort", USHORT},
    {"using", USING},
    {"virtual", VIRTUAL},
    {"void", VOID},
    {"volatile", VOLATILE},
    {"while", WHILE},
  };    


/* Conduct a binary search for lexeme in the keywords array
 * between indices start (inclusive) and finish (exclusive)
 */
static int bin_search(const char *lexeme, int start, int finish)
{
  if (start >= finish)  /* Not found */
      return IDENTIFIER;
  else {
    int mid = (start+finish)/2;
    int cmp = strcmp(lexeme,keywords[mid].name);
    if (cmp == 0) 
      return keywords[mid].value;
    else if (cmp < 0)
      return bin_search(lexeme,start,mid);
    else
      return bin_search(lexeme,mid+1,finish);
  }
}

static int token_for(const char *lexeme)
{

  static int num_keywords = sizeof(keywords) / sizeof(name_value);
  int token = bin_search(lexeme,0,num_keywords);
  if (token == -1)
    token = IDENTIFIER;
  return token;
}

static int is_allowed_char(const char c)
{
  static char allowed [] = {
    '.', ';', ':',
    '(', ')', '{', '}', '<', '>',
    '+', '-', '*', '/', '%', '&', '|', '!', '~', '^',
    '=', '?',
  };
  static int num_allowed = sizeof(allowed) / sizeof(char);
  int i;
  for (i=0; i<num_allowed; i++) {
    if (allowed[i]==c) 
      return 1;
  }
  return 0;
}

void lexical_error(const char *msg)
{
  fprintf(stderr,"%d:lexical error (%s) [%s]\n",yylineno,msg,yytext);
}


/*** Switch on/off context-sensitive "keywords" ***/
/* I don't like it any more than you do.  */

void lex_enter_attrib(void)
{
  yy_push_state(IN_ATTRIB);
}
void lex_exit_attrib(void)
{
  yy_pop_state();
}
void lex_enter_accessor(void)
{
  yy_push_state(IN_ACCESSOR);
}
void lex_exit_accessor(void)
{
  yy_pop_state();
}
void lex_enter_getset(void)
{
  yy_push_state(IN_GETSET);
}
void lex_exit_getset(void)
{
  yy_pop_state();
}
