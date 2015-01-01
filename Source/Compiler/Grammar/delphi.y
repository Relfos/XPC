%start goal
    
%token KW_LIBRARY KW_UNIT  KW_PROGRAM
%token KW_PACKAGE KW_REQUIRES KW_CONTAINS
%token KW_USES KW_EXPORTS
%token KW_PLATFORM KW_DEPRECATED
%token KW_INTERF KW_IMPL KW_FINALIZ KW_INIT
%token KW_OBJECT KW_RECORD KW_CLASS
%token KW_FUNCTION KW_PROCEDURE KW_PROPERTY
%token KW_OF KW_OUT KW_PACKED KW_INHERITED
%token KW_PROTECTED KW_PUBLIC KW_PUBLISHED KW_PRIVATE
%token KW_CONST KW_VAR KW_THRVAR KW_TYPE KW_CONSTRUCTOR KW_DESTRUCTOR KW_ASM
%token KW_BEGIN KW_END KW_WITH KW_DO
%token KW_FOR KW_TO KW_DOWNTO KW_REPEAT KW_UNTIL KW_WHILE
%token KW_IF KW_THEN KW_ELSE KW_CASE KW_GOTO KW_LABEL KW_BREAK KW_CONTINUE
%token KW_RAISE KW_AT KW_TRY KW_EXCEPT KW_FINALLY KW_ON
%token KW_ABSOLUTE KW_ABSTRACT KW_ASSEMBLER KW_DYNAMIC KW_EXPORT KW_EXTERNAL KW_FORWARD KW_INLINE KW_OVERRIDE KW_OVERLOAD KW_REINTRODUCE KW_VIRTUAL KW_VARARGS
%token KW_PASCAL KW_SAFECALL KW_STDCALL KW_CDECL KW_REGISTER
%token TYPE_WIDESTR TYPE_STR TYPE_RSCSTR TYPE_SHORTSTR  TYPE_ARRAY TYPE_FILE TYPE_PTR TYPE_SET
%token KW_NAME KW_READ KW_WRITE KW_INDEX KW_STORED KW_DEFAULT KW_NODEFAULT KW_IMPLEMENTS
%token ASM_OP WINDOWS_GUID  KW_FAR KW_NEAR KW_RESIDENT 

%token TYPE_INT64 TYPE_INT TYPE_LONGINT TYPE_LONGWORD TYPE_SMALLINT TYPE_SHORTINT TYPE_WORD TYPE_BYTE TYPE_CARDINAL TYPE_UINT64
%token TYPE_CHAR TYPE_PCHAR TYPE_WIDECHAR TYPE_WIDESTR TYPE_STR TYPE_RSCSTR TYPE_SHORTSTR
%token TYPE_FLOAT TYPE_REAL48 TYPE_DOUBLE TYPE_EXTENDED
%token TYPE_BOOL TYPE_COMP TYPE_CURRENCY TYPE_OLEVAR TYPE_VAR TYPE_ARRAY TYPE_CURR TYPE_FILE TYPE_PTR TYPE_SET

%token ASM_OP

%nonassoc LOWESTPREC 
%right KW_THEN 
%right KW_ELSE 
%token <StringObject> PASCAL_IDENTIFIER CONST_STR
%token <Int64> CONST_INT  CONST_NIL
%token <Double> CONST_REAL 
%token <AnsiChar> CONST_CHAR 
%token <Boolean> CONST_BOOL
%nonassoc KW_RANGE COMMA COLON SCOL KW_ASSIGN
%left KW_EQ KW_GT KW_LT KW_LE KW_GE KW_NE KW_IN KW_IS
%left KW_SUM KW_SUB KW_OR KW_XOR
%left KW_MUL KW_DIV KW_QUOT KW_MOD KW_SHL KW_SHR KW_AS KW_AND

%left KW_DEREF KW_DOT

%left UNARY KW_NOT KW_ADDR

%nonassoc LBRAC RBRAC LPAR RPAR

%nonassoc MAXPREC


%type <StringObject> id qualifid idtypeopt labelid stringconst stringorchar stringnonNil conststr expid programid
%type <StringList> labelidlst idlst varids

%type <SourceNode> goal file program library unit package 
%type <UnitItem> containsitem usesitem expitem 
%type <NodeList> usesopt useslst requires requireslst containslst contains 
%type <DeclarationList> interfdecllst interfdecl maindecllst declseclst maindeclsec funcdeclsec basicdeclsec expitemlst
%type <DeclarationList> thrvarsec rscstringsec exportsec labeldeclsec constsec
%type <DeclarationList> typesec varsec rscstringlst vardecl objfield
%type <ImplementationSection> implsec
%type <InterfaceSection> interfsec
%type <BlockStatement> initsec finalsec
%type <ConstDeclaration> constdecl rscstring
%type <TypeDeclaration> typedecl
%type <RoutineSection> funcdefine

%type <CallableDeclaration> routinedeclmain
%type <RoutineDeclaration> routineproto routinedeclinterf
%type <MethodDeclaration> methoddecl classmetdecl interfmetdecl 
%type <MethodDefinition> methoddef metdefproto
%type <RoutineDefinition> routinedef
%type <FunctionDirectiveList> funcdirectlst funcdir_noterm_opt funcdiropt importdirforced  metdirectopt smetdirslst smetdirs metdirectlst  
%type <FunctionDirective> funcdirective smetqualif metdirective metqualif funcqualif funcdeprecated routinecallconv
%type <MethodKind> kwmetspec
%type <DeclarationList> formalparamslst formalparm
%type <ParametersSection> formalparams
%type <ExternalDirective> externarg
%type <StringObject> kwfunction kwprocedure

%type <BlockStatement> block funcblock assemblerstmt blockstmt
%type <ExceptionBlock> exceptionblock
%type <Statement> stmt nonlblstmt assign goto_stmt ifstmt casestmt elsecase caseselector 
%type <Statement> tryexceptstmt tryfinallystmt raisestmt ondef repeatstmt whilestmt forstmt withstmt 
%type <StatementList> casesellst onlst stmtlist asmcode

%type <Literal> literal basicliteral discrete strorcharlit
%type <StringLiteral> stringlit
%type <Integer> mulop addop relop fordir
%type <Cardinal> constint constnil
%type <Boolean>   constbool
%type <Double> constreal
%type <AnsiChar>   constchar
%type <LvalueExpression> lvalue lvalstmt
%type <Identifier> identifier
%type <Expression> unaryexpr expr rangestart set setelem constexpr constinit paraminitopt lvalasval
%type <ExpressionList> caselabellst exprlst constexprlst exprlstopt setelemlst arrexprlst callparams
%type <TypeList> arrayszlst
%type <FieldInitList> fieldconstlst 
%type <FieldInit> fieldconst
%type <EnumValueList> enumelemlst
%type <EnumValue> enumelem
%type <ConstExpression> functypeinit arrayconst recordconst

%type <DeclarationList> recvariant recfield recvarfield recfieldlst propfield recvarlst 
%type <DeclarationList> ccomplstopt fieldlst cfieldlst classcomplst interfcomplst arrayprops
%type <StringList> heritage 
%type <Declaration> classcomp property interfcomp
%type <InterfaceType> packinterftype interftype
%type <ClassTypeNode> classtype packclasstype
%type <StringLiteral> guid
%type <Integer> scope
%type <Boolean> staticopt defaultdiropt
%type <ObjectSection> classbody interfbody class1stsec scopeseclst
%type <StringObject> implopt readopt writeopt
%type <IntLiteral> indexopt 
%type <Literal> defaultopt 
%type <ConstExpression> storeopt
%type <PropertySpecifiers> spropspecsnormal spropspecsoverride spropspecsarray

%type <TypeNode> paramtypeopt paramtypespec funcpartype  
%type <TypeNode> enumtype rangetype ordinaltype fixedtype stringtype
%type <TypeNode> vartype funcrettype funcret
%type <StructuredType> structuredtype packstructtype recordtype  arraytype settype filetype
%type <ProceduralType> proceduraltype proceduralsign
    

%%


    
goal: file KW_DOT                           { $$ := $1; yyflag := yyfaccept; }
    ;

file
    : program                               { $$ := $1; }
    | KW_PACKAGE package KW_END             { $$ := $2; }
    | KW_LIBRARY library                    { $$ := $2; }
    | KW_UNIT unit KW_END                   { $$ := $2; }
    ;

scolopt
    :
    | SCOL
    ;
    

    
program
    : programid usesopt maindecllst block   { $$ := ProgramNode.Create($1, $2,   $3, $4); }
    | programid usesopt             block   { $$ := ProgramNode.Create($1, $2, Nil, $3); }
    ;
    
programid
    :                                       { $$ := Nil; }
    | KW_PROGRAM id SCOL                    { $$ := $2;   }
    ; 
    
library
    : id SCOL usesopt maindecllst block     { $$ := LibraryNode.Create($1, $3, $4  , $5); }
    | id SCOL usesopt             block     { $$ := LibraryNode.Create($1, $3, Nil, $4); }
    ;
    
package
    : id SCOL requires contains             { $$ := PackageNode.Create($1, $3, $4); }
    ;

requires
    :                                       { $$ := NodeList.Create(); }
    | KW_REQUIRES requireslst SCOL          { $$ := $2; }
    ;
    
requireslst
    : id                                    { $$ := NodeList.Create(); $$.Add($1); }
    | requireslst COMMA id                  { $1.Add($3); $$ := $1; }
    ;

contains
    : KW_CONTAINS containslst SCOL          { $$ := $2; }
    ;

containslst
    : containsitem                          { $$ := NodeList.Create(); $$.Add($1); }
    | containslst COMMA containsitem        { $1.Add($3); $$ := $1; }
    ;

containsitem
    : id                                    { $$ := UnitItem.Create($1); }
    | id KW_IN stringnonNil                { $$ := UnitItem.Create($1, $3); }
    ;

usesopt
    :                                       { $$ := NodeList.Create(); }
    | KW_USES useslst SCOL                  { $$ := $2; }
    ;

useslst
    : usesitem                              { $$ := NodeList.Create(); $$.Add($1); }
    | useslst COMMA usesitem                { $1.Add($3); $$ := $1; }
    ;
    
usesitem
    : id                                    { $$ := UnitItem.Create($1); }
    | id KW_IN stringnonNil                { $$ := UnitItem.Create($1, $3);}
    ;

unit
    : id SCOL interfsec implsec
        initsec finalsec                    { $$ := UnitNode.Create($1, $3, $4, $5, $6); }
    | id SCOL interfsec implsec initsec     { $$ := UnitNode.Create($1, $3, $4, $5, Nil); }
    | id SCOL interfsec implsec finalsec    { $$ := UnitNode.Create($1, $3, $4, Nil, $5); }
    | id SCOL interfsec implsec             { $$ := UnitNode.Create($1, $3, $4); }
    ;

implsec
    : KW_IMPL usesopt   maindecllst         { $$ := ImplementationSection.Create($2, $3);}
    | KW_IMPL usesopt                       { $$ := ImplementationSection.Create($2, Nil);}
    ;

interfsec
    : KW_INTERF usesopt interfdecllst       { $$ := InterfaceSection.Create($2, $3); }
    ;

interfdecllst
    :                                       { $$ := DeclarationList.Create();}
    | interfdecllst interfdecl              { $$ := $1; $1.Add($2); }
    ;

initsec
    : KW_INIT  blockstmt                    { $$ := $2;}
    | KW_BEGIN blockstmt                    { $$ := $2;}
    ;
    
finalsec
    : KW_FINALIZ blockstmt                  { $$ := $2;}
    ;
        
maindecllst
    : maindeclsec                           { $$ := $1; }
    | maindecllst maindeclsec               { $$ := $1; $1.Add($2); }
    ;

declseclst
    : funcdeclsec                           { $$ := $1;}
    | declseclst funcdeclsec                { $$ := $1; $1.Add($2); }
    ;

    


interfdecl
    : basicdeclsec          { $$ := $1;}
    | routinedeclinterf     { $$ := DeclarationList.Create; $$.Add($1);}
    | thrvarsec             { $$ := $1;}
    | rscstringsec          { $$ := $1;}
    ;

maindeclsec
    : basicdeclsec          { $$ := $1;}
    | thrvarsec             { $$ := $1;}
    | exportsec             { $$ := $1;}
    | routinedeclmain       { $$ := DeclarationList.Create(); $$.Add($1);}
    | labeldeclsec          { $$ := $1;}
    ;

funcdeclsec
    : basicdeclsec          { $$ := $1;}
    | labeldeclsec          { $$ := $1;}
    | routinedef            { $$ := DeclarationList.Create(); $$.Add($1);}
    ;

basicdeclsec
    : constsec              { $$ := $1;}
    | typesec               { $$ := $1;}
    | varsec                { $$ := $1;}
    ;

typesec
    : KW_TYPE typedecl      { $$ := DeclarationList.Create(); $$.Add($2); }
    | typesec typedecl      { $1.Add($2);  $$ := $1; }
    ;


varsec
    : KW_VAR vardecl        { $$ := $2; }
    | varsec vardecl        { $$ := $1; $1.Add($2); }
    ;
    
thrvarsec
    : KW_THRVAR vardecl     { $$ := MakeThreadVars($2); }
    | thrvarsec vardecl     { $$ := $1; $1.Add($2); MakeThreadVars($$); }                                
    ;

vardecl
    : varids vartype SCOL                       { $$ := CreateDecls($1, $2, Nil, Nil); }
    | varids vartype KW_EQ constinit SCOL       { $$ := CreateDecls($1, $2, $4, Nil); }
    | varids vartype KW_ABSOLUTE id SCOL        { $$ := CreateDecls($1, $2, Nil, $4); }
    | varids proceduraltype                     { $$ := CreateDecls($1, $2, Nil, Nil); }
    | varids proceduralsign SCOL funcdir_noterm_opt functypeinit SCOL    { RaiseError('TODO'); (*$$ := CreateDecls($1, $2, $5))); $2.Directives := $4;*) }
    ;

varids
    : idlst COLON                   { $$ := $1; }
    ;

rscstringsec
    : TYPE_RSCSTR rscstringlst      { $$ := $2; }
    ;
    
rscstringlst
    : rscstring                     { $$ := DeclarationList.Create(); $$.Add($1); }
    | rscstringlst rscstring        { $1.Add($2); $$ := $1; }
    ;
    
rscstring
    :  id KW_EQ stringlit SCOL      { $$ := ConstDeclaration.Create($1, $3); }
    ;

    
labeldeclsec
    : KW_LABEL labelidlst SCOL      { $$ := CreateLabelDecls($2); }
    ;
    
labelidlst 
    : labelid                       { $$ := StringList.Create(); $$.Add($1); }
    | labelidlst COMMA labelid      { $$ := $1; $1.Add($3); }
    ;

labelid
    : constint                      {   (* decimal int 0..9999 *)
                                        If ($1 < 0) Or ($1 > 9999) Then
                                        Begin
                                            yyerror('Label number must be between 0 and 9999');
                                        End;
                                        $$ := StringObject.Create(CardinalToString($1));
                                    }
    | id                            { $$ := $1; }
    ;


exportsec   
    : KW_EXPORTS expitemlst         { $$ := $2; }
    ;

expitemlst
    : expitem                       { $$ := DeclarationList.Create($1); }
    | expitemlst COMMA expitem      { $$ := $1; $1.Add($3); }
    ;

expitem
    : expid formalparams                        { $$ := ExportItem.Create($1, $2); }
    | expid formalparams KW_NAME stringnonNil  { $$ := ExportItem.Create($1, $2, $4); }
    | expid formalparams KW_INDEX constint      { $$ := ExportItem.Create($1, $2, $4); }
    ;
    
expid
    : id                            { $$ := $1; }
    | id KW_DOT id                  { $$ := StringObject.Create($1.Value + '.' + $3.Value); // possible mem leak}
    ;


kwclass
    : KW_CLASS          { }
    | KW_OBJECT         { }
    ;



routinedeclmain                                         
    : routinedef                                { $$ := $1; }
    | routineproto importdirforced              { $$ := $1; $1.Directives := $2; }
    | methoddef funcdefine SCOL                 { $$ := $1; $1.body := $2; }
    ;

routinedef
    : routineproto funcdiropt funcdefine SCOL   { $$ := RoutineDefinition.Create($1.name, $1.SignatureType, $2, $3); }
    ;

methoddecl
    : kwfunction  formalparams funcret SCOL     { $$ := MethodDeclaration.Create(_lastObjName, $1, $2, $3); }
    | kwprocedure formalparams         SCOL     { $$ := MethodDeclaration.Create(_lastObjName, $1, $2, Nil); }
    ;
    
routineproto
    : kwfunction  formalparams funcret  SCOL    { $$ := RoutineDeclaration.Create($1, $2, $3); }
    | kwprocedure formalparams          SCOL    { $$ := RoutineDeclaration.Create($1, $2); }
    ;   

methoddef
    :           metdefproto SCOL metdirectopt   { $$ := $1; $1.Directives := $3; }
    | KW_CLASS  metdefproto SCOL metdirectopt   { $$ := $2; $2.Directives := $4; $2.isStatic := true; }
    | kwmetspec id KW_DOT id formalparams SCOL smetdirs { $$ := MethodDefinition.Create($2, $4, $5, Nil, $7, $1);}
    ;

metdefproto
    : kwfunction KW_DOT id formalparams funcret { $$ := MethodDefinition.Create($1, $3, $4, $5); }
    | kwprocedure KW_DOT id formalparams        { $$ := MethodDefinition.Create($1, $3, $4); }
    ;
    
routinedeclinterf
    : routineproto                              { $$ := $1; }
    ;

classmetdecl
    : methoddecl metdirectopt                   { $$ := $1; $1.Directives := $2; }
    | kwmetspec id formalparams SCOL smetdirs   { $$ := MethodDeclaration.Create(_lastObjName, $2, $3, Nil, $5, $1); }
    ;

interfmetdecl
    : methoddecl                                { $$ := $1; }
    ;

kwfunction
    : KW_FUNCTION  id                           { $$ := $2; }
    ;
kwprocedure
    : KW_PROCEDURE id                           { $$ := $2; }
    ;
kwmetspec
    : KW_CONSTRUCTOR                            { $$ := method_Constructor; }
    | KW_DESTRUCTOR                             { $$ := method_Destructor; }
    ;
    
proceduraltype
    : proceduralsign SCOL funcdiropt            { $$ := $1; $1.Directives := $3; }
    ;
    
proceduralsign
    : KW_PROCEDURE formalparams                 { $$ := ProceduralType.Create($2); }
    | KW_FUNCTION  formalparams funcret         { $$ := ProceduralType.Create($2, $3); } 
    | KW_PROCEDURE formalparams typeobj         { $$ := MethodType.Create($2); } 
    | KW_FUNCTION  formalparams funcret typeobj { $$ := MethodType.Create($2, $3); } 
    ;

typeobj
    : KW_OF KW_OBJECT
    ;
    
funcret
    : COLON funcrettype                         { $$ := $2;}
    ;


funcdefine
    : declseclst funcblock                      { $$ := RoutineSection.Create($1, $2); }
    |            funcblock                      { $$ := RoutineSection.Create(Nil, $1); }
    ;

funcblock
    : block                                     { $$ := $1; }
    | assemblerstmt                             { $$ := $1; }
    ;

formalparams
    :                                           { $$ := ParametersSection.Create(); }
    | LPAR RPAR                                 { $$ := ParametersSection.Create(); }
    | LPAR formalparamslst RPAR                 { $$ := ParametersSection.Create($2); }
    ;

formalparamslst
    : formalparm                                { $$ := $1; }
    | formalparamslst SCOL formalparm           { $$ := $1; $1.Add($3); }
    ;


formalparm
    : KW_VAR   idlst paramtypeopt               { $$ := CreateParamDecls($2, $3, Nil, param_Var); } 
    | KW_OUT   idlst paramtypeopt               { $$ := CreateParamDecls($2, $3, Nil, param_Out); } 
    |          idlst paramtypespec paraminitopt { $$ := CreateParamDecls($1, $2, $3, param_Default); }
    | KW_CONST idlst paramtypeopt  paraminitopt { $$ := CreateParamDecls($2, $3, $4, param_Const); }
    ;

paramtypeopt
    :                                           { $$ := Nil; }
    | paramtypespec                             { $$ := $1; }
    ;

paramtypespec
    : COLON funcpartype                         { $$ := $2; }
    ;

paraminitopt
    :                                           { $$ := Nil; }
    | KW_EQ constexpr                           { $$ := $2; }
    ;

functypeinit
    : KW_EQ id                                  { $$ := ConstIdentifier.Create($2); }
    | KW_EQ constnil                            { $$ := PointerLiteral.Create(0); }
    ;

    
importdirforced
    : funcdiropt KW_EXTERNAL externarg SCOL funcdiropt  { $$ := JoinImportDirectives($1, $5, $3); }
    | funcdiropt KW_FORWARD SCOL funcdiropt             { $$ := JoinImportDirectives($1, $4, importDirective_Forward); }
    ;

externarg
    : constexpr KW_NAME constexpr       { $$ := ExternalDirective.Create($1, $3); }
    | constexpr                         { $$ := ExternalDirective.Create($1); }
    |                                   { $$ := Nil; }
    ;

funcdir_noterm_opt
    :                                   { $$ := Nil ; }
    | funcdirectlst                     { $$ := $1   ; }
    ;

funcdiropt
    :                                   { $$ := Nil ; }
    | funcdirectlst SCOL                { $$ := $1   ; }
    ;

metdirectopt
    :                                   { $$ := Nil ; }
    | metdirectlst SCOL                 { $$ := $1; }
    ;

smetdirs
    :                                   { $$ := Nil; }
    | smetdirslst SCOL                  { $$ := $1   ; }
    ;

funcdirectlst
    : funcdirective                     { $$ := FunctionDirectiveList.Create($1); }
    | funcdirectlst SCOL funcdirective  { $$ := $1; $1.Add($3); }
    ;

smetdirslst
    : smetqualif                        { $$ := FunctionDirectiveList.Create($1); }
    | smetdirslst SCOL smetqualif       { $$ := $1; $1.Add($3); }
    ;

metdirectlst
    : metdirective                      { $$ := FunctionDirectiveList.Create($1); }
    | metdirectlst SCOL metdirective    { $$ := $1; $1.Add($3); }
    ;

smetqualif
    : KW_OVERLOAD           { $$ := functionDirective_Overload; }
    | metqualif             { $$ := $1; }
    ;
    
metdirective
    : funcdirective         { $$ := $1; }
    | metqualif             { $$ := $1; }
    ;
    
funcdirective
    : funcqualif            { $$ := $1; }
    | routinecallconv       { $$ := $1; }
    | funcdeprecated        { $$ := $1; }
    ;

    
funcdeprecated
    : KW_FAR                { $$ := functionDirective_Far; }
    | KW_NEAR               { $$ := functionDirective_Near; }
    | KW_RESIDENT           { $$ := functionDirective_Resident; }
    ;

metqualif
    : KW_ABSTRACT           { $$ := methodDirective_Abstract; }
    | KW_DYNAMIC            { $$ := methodDirective_Dynamic; }
    | KW_OVERRIDE           { $$ := methodDirective_Override; }
    | KW_VIRTUAL            { $$ := methodDirective_Virtual; }
    | KW_REINTRODUCE        { $$ := methodDirective_Reintroduce; }
    ;

funcqualif
    : KW_ASSEMBLER          { $$ := functionDirective_Assembler; }
    | KW_EXPORT             { $$ := functionDirective_Export; }
    | KW_INLINE             { $$ := functionDirective_Inline; }
    | KW_OVERLOAD           { $$ := functionDirective_Overload; }
    | KW_VARARGS            { $$ := functionDirective_VarArgs; }
    ;
    
routinecallconv
    : KW_PASCAL             { $$ := callConvention_Pascal; }
    | KW_SAFECALL           { $$ := callConvention_SafeCall; }
    | KW_STDCALL            { $$ := callConvention_StdCall; }
    | KW_CDECL              { $$ := callConvention_CDecl; }
    | KW_REGISTER           { $$ := callConvention_Register; }
    ;
    

block
    : KW_BEGIN blockstmt KW_END         { $$ := $2; }
    ;

blockstmt
    : stmtlist                          { $$ := BlockStatement.Create(StatementList.Create($1)); }
    ;
    
stmtlist
    : stmt                              { $$ := StatementList.Create($1); }
    | stmt SCOL stmtlist                { $$ := $3; $3.Add($1); }
    ;

stmt
    : nonlblstmt                        { $$ := $1; }
    | labelid COLON nonlblstmt          { $$ := LabelStatement.Create($1, $3); }
    ;

nonlblstmt
    :                                   { $$ := EmptyStatement.Create(); }
    | lvalstmt                          { $$ := ExpressionStatement.Create($1); }
    | assign                            { $$ := $1; }
    | goto_stmt                         { $$ := $1; }
    | block                             { $$ := $1; }
    | ifstmt                            { $$ := $1; }
    | casestmt                          { $$ := $1; }
    | repeatstmt                        { $$ := $1; }
    | whilestmt                         { $$ := $1; }
    | forstmt                           { $$ := $1; }
    | withstmt                          { $$ := $1; }
    | tryexceptstmt                     { $$ := $1; }
    | tryfinallystmt                    { $$ := $1; }
    | raisestmt                         { $$ := $1; }
    | assemblerstmt                     { $$ := $1; }
    | KW_BREAK                          { $$ := BreakStatement.Create(); }
    | KW_CONTINUE                       { $$ := ContinueStatement.Create(); }
    ;

assign
    : lvalue KW_ASSIGN expr             { $$ := Assignment.Create($1, $3); }
    ;

goto_stmt
    : KW_GOTO labelid                   { $$ := GotoStatement.Create($2); }
    ;

ifstmt
    : KW_IF expr KW_THEN nonlblstmt 
        KW_ELSE nonlblstmt              { $$ := IfStatement.Create($2, $4, $6); }
    | KW_IF expr KW_THEN nonlblstmt     { $$ := IfStatement.Create($2, $4, Nil); }
    ;

casestmt
    : KW_CASE expr KW_OF casesellst
        elsecase KW_END                 { $$ := CaseStatement.Create($2, $4, $5); }
    ;

elsecase
    :                                   { $$ := Nil;}   
    | KW_ELSE nonlblstmt                { $$ := $2; }
    | KW_ELSE nonlblstmt SCOL           { $$ := $2; }
    ;

casesellst
    : caseselector                      { $$ := StatementList.Create($1); }
    | casesellst SCOL caseselector      { $$ := $1; $1.Add($3); }
    ;

caseselector
    :                                   { $$ := Nil; }
    | caselabellst COLON nonlblstmt     { $$ := CaseSelector.Create($1, $3); }
    ;
    
caselabellst
    : setelem                           { $$ := ExpressionList.Create($1); }
    | caselabellst COMMA setelem        { $$ := $1; $1.Add($3); }
    ;

repeatstmt
    : KW_REPEAT blockstmt KW_UNTIL expr { $$ := RepeatLoop.Create($2, $4); }
    ;

whilestmt
    : KW_WHILE expr KW_DO nonlblstmt    { $$ := WhileLoop.Create($4, $2); }
    ;

forstmt
    : KW_FOR identifier KW_ASSIGN expr
        fordir expr KW_DO nonlblstmt    { $$ := ForLoop.Create($2, $4, $6, $8, $5); }
    ;

fordir
    : KW_TO                             { $$ := 1; }
    | KW_DOWNTO                         { $$ := -1; }
    ;

withstmt
    : KW_WITH exprlst KW_DO nonlblstmt  { $$ := WithStatement.Create($2, $4); }
    ;

tryexceptstmt
    : KW_TRY blockstmt KW_EXCEPT
        exceptionblock KW_END           { $$ := TryExceptStatement.Create($2, $4); }
    ;

exceptionblock
    : onlst KW_ELSE blockstmt           { $$ := ExceptionBlock.Create($1, $3); }
    | onlst                             { $$ := ExceptionBlock.Create($1); }
    | blockstmt                         { $$ := ExceptionBlock.Create(Nil, $1); }
    ;

onlst
    : ondef                             { $$ := StatementList.Create($1); }
    | onlst ondef                       { $$ := $1; $1.Add($2); }
    ;

ondef
    : KW_ON id COLON id KW_DO nonlblstmt SCOL   { $$ := OnStatement.Create($2, $4, $6); }
    | KW_ON          id KW_DO nonlblstmt SCOL   { $$ := OnStatement.Create(Nil, $2, $4); }
    ;

tryfinallystmt
    : KW_TRY blockstmt 
        KW_FINALLY blockstmt KW_END     { $$ := TryFinallyStatement.Create($2, $4); }
    ;

raisestmt
    : KW_RAISE                          { $$ := RaiseStatement.Create(Nil, Nil); }
    | KW_RAISE lvalue                   { $$ := RaiseStatement.Create($2, Nil); }
    | KW_RAISE          KW_AT expr      { $$ := RaiseStatement.Create(Nil, $3); }
    | KW_RAISE lvalue   KW_AT expr      { $$ := RaiseStatement.Create($2, $4); }
    ;

assemblerstmt
    : KW_ASM asmcode KW_END             { $$ := AssemblerBlock.Create($2); }
    ;

asmcode
    :                                   { $$ := Nil; }
    | asmcode ASM_OP                    { $$ := Nil; }
    ;

identifier
    : id                                { $$ := Identifier.Create($1); }
    ;
    
lvalstmt
    : identifier                        { $$ := UnresolvedId.Create($1); }
    | lvalue LPAR exprlstopt RPAR       { $$ := UnresolvedCall.Create($1, $3); }
    | TYPE_PTR LPAR expr RPAR           { $$ := StaticCast.Create(PointerType, $3); }
    | lvalue KW_DOT id                  { $$ := ObjectAccess.Create($1, $3); }
    | KW_INHERITED                      { $$ := InheritedCall.Create(Nil); }
    | KW_INHERITED id callparams        { $$ := InheritedCall.Create($2, $3); }
    ;
    
lvalue 
    : identifier                        { $$ := UnresolvedId.Create($1); }
    | lvalue LPAR exprlstopt RPAR       { $$ := UnresolvedCall.Create($1, $3); }
    | TYPE_PTR LPAR expr RPAR           { $$ := StaticCast.Create(PointerType, $3); }
    | lvalue KW_DOT id                  { $$ := ObjectAccess.Create($1, $3); }
    | lvalue KW_DEREF                   { $$ := PointerDereference.Create($1); }
    | lvalue LBRAC exprlst RBRAC        { $$ := ArrayAccess.Create($1, $3); }
    | LPAR expr RPAR                    { $$ := ExprAsLvalue.Create($2); }
    ;

lvalasval
    : lvalue                            { If ($1 Is ExprAsLvalue) Then $$ := ($1 as ExprAsLvalue).expr Else $$ := LvalueAsExpr.Create($1); }
    ;

unaryexpr
    : literal                           { $$ := $1; }
    | lvalasval                         { $$ := $1; } 
    | set                               { $$ := $1; }
    | KW_ADDR lvalue                    { $$ := AddressLvalue.Create($2); }
    | KW_NOT unaryexpr                  { $$ := LogicalNot.Create($2); }
    | KW_SUM unaryexpr                  { $$ := UnaryPlus.Create($2); }
    | KW_SUB unaryexpr                  { $$ := UnaryMinus.Create($2); }
    | KW_INHERITED                      { $$ := InheritedCall.Create(Nil); }
    | KW_INHERITED id callparams        { $$ := InheritedCall.Create($2, $3); }
    | stringnonNil LBRAC expr RBRAC    { $$ := ArrayAccess.Create(ArrayConst.Create($1), ExpressionList.Create($3)); }
    ;

callparams
    :                                   { $$ := ExpressionList.Create(); }
    | LPAR exprlstopt RPAR              { $$ := $2; }
    ;

expr
    : unaryexpr                         { $$ := $1; }
    | expr KW_AS qualifid               { $$ := RuntimeCast.Create($1, ClassRefType.Create($3)); }
    | expr KW_IS qualifid               { $$ := TypeIs.Create($1, ClassRefType.Create($3)); }
    | expr KW_IN expr                   { $$ := SetIn.Create($1, $3); }
    | expr relop expr %prec KW_EQ       { $$ := CreateBinaryExpression($1, $2, $3); }
    | expr addop expr %prec KW_SUB      { $$ := CreateBinaryExpression($1, $2, $3); }
    | expr mulop expr %prec KW_MUL      { $$ := CreateBinaryExpression($1, $2, $3); }
    ;

mulop
    : KW_MUL        { $$ := KW_MUL; }
    | KW_DIV        { $$ := KW_DIV; }
    | KW_QUOT       { $$ := KW_QUOT; }
    | KW_MOD        { $$ := KW_MOD; }
    | KW_SHR        { $$ := KW_SHR; }
    | KW_SHL        { $$ := KW_SHL; }
    | KW_AND        { $$ := KW_AND; }
    ;
addop
    : KW_SUB        { $$ := KW_SUB; }
    | KW_SUM        { $$ := KW_SUM; }
    | KW_OR         { $$ := KW_OR; }
    | KW_XOR        { $$ := KW_XOR; }
    ;
relop
    : KW_EQ         { $$ := KW_EQ; }
    | KW_NE         { $$ := KW_NE; }
    | KW_LT         { $$ := KW_LT; }
    | KW_LE         { $$ := KW_LE; }
    | KW_GT         { $$ := KW_GT; }
    | KW_GE         { $$ := KW_GE; }
    ;

exprlst
    : expr          { $$ := ExpressionList.Create($1); }
    | exprlst COMMA expr  { $$ := $1; $1.Add($3); }
    ;

exprlstopt
    :               { $$ := ExpressionList.Create(); }
    | exprlst       { $$ := $1; }
    ;
   
literal
    : basicliteral  { $$ := $1; }
    | strorcharlit  { $$ := $1; }
    ;

basicliteral
    : constint      { $$ := IntLiteral.Create($1);}
    | constbool     { $$ := BoolLiteral.Create($1);}
    | constreal     { $$ := RealLiteral.Create($1);}
    | constnil      { $$ := PointerLiteral.Create($1);}
    ;

discrete
    : constint      { $$ := IntLiteral.Create($1);}
    | constchar     { $$ := CharLiteral.Create($1);}
    | constbool     { $$ := BoolLiteral.Create($1);}
    ;

strorcharlit
    : stringorchar  { If (Length($1.Value)=1) Then $$ := CharLiteral.Create($1.Value[1]) Else $$ := StringLiteral.Create($1); }
    ;

stringlit
    : stringconst   { $$ := StringLiteral.Create($1); }
    ;

stringorchar
    : conststr      { $$ := $1; }
    | constchar     { $$ := StringObject.Create($1); }
    | stringorchar conststr     { $$ := StringObject.Create($1.Value + $2.Value); }
    | stringorchar constchar    { $$ := StringObject.Create($1.Value + $2); }
    ;
    
stringconst
    : stringorchar  { $$ := $1; if (Length($1.Value) = 1) Then yyerror('Expected string, found char'); }
    ;

stringnonNil
    : stringconst   { $$ := $1; If (Length($1.Value) <= 0) Then yyerror('Invalid empty string'); }
    ;
    
idlst
    : id            { $$ := StringList.Create(); $$.Add($1); }
    | idlst COMMA id{ $$ := $1; $1.Add($3); }
    ;

id  : PASCAL_IDENTIFIER    { $$ := $1; }
    ;
    
constnil
    : CONST_NIL     { $$ := 0; }
    ; 
constint
    : CONST_INT     { $$ := $1; }
    ;
constchar
    : CONST_CHAR    { $$ := $1;}   
    ;
constreal
    : CONST_REAL    { $$ := $1;}
    ;
constbool
    : CONST_BOOL    { $$ := $1;}
    ;
conststr
    :  CONST_STR    { $$ := $1; }
    ;

    

rangetype           
    : rangestart KW_RANGE expr      { $$ := RangeType.Create($1, $3); $1.EnforceConst := True; $3.EnforceConst := True; }
    ;
    
rangestart
    : discrete                      { $$ := $1; }
    | KW_SUM unaryexpr              { $$ := UnaryPlus.Create($2); }
    | KW_SUB unaryexpr              { $$ := UnaryMinus.Create($2); }
    ;

enumtype
    : LPAR enumelemlst RPAR         { $$ := EnumType.Create($2); }
    ;

enumelemlst
    : enumelem                      { $$ := EnumValueList.Create($1); }
    | enumelemlst COMMA enumelem    { $$ := $1; $1.Add($3); }
    ;

enumelem
    : id                            { $$ := EnumValue.Create($1); }
    | id KW_EQ expr                 { $$ := EnumValue.Create($1, $3); }
    ;

set
    : LBRAC RBRAC                   { $$ := SetExpression.Create();}
    | LBRAC setelemlst  RBRAC       { $$ := SetExpression.Create($2); }
    ;

setelemlst
    : setelem                       { $$ := ExpressionList.Create(); $$.Add($1); }
    | setelemlst COMMA setelem      { $$ := $1; $1.Add($3); }
    ;
    
setelem
    : constexpr                     { $$ := $1; }
    | constexpr KW_RANGE constexpr  { $$ := SetRange.Create(RangeType.Create($1, $3)); }
    ;


constsec
    : KW_CONST constdecl            { $$ := DeclarationList.Create(); $$.Add($2); }
    | constsec constdecl            { $$ := $1; $1.Add($2); }
    ;

constdecl
    : id KW_EQ constexpr SCOL                   { $$ := ConstDeclaration.Create($1, $3); }  
    | id COLON vartype KW_EQ constinit SCOL     { $$ := ConstDeclaration.Create($1, $5, $3); }
    | id COLON proceduralsign
        funcdir_noterm_opt functypeinit SCOL    { $$ := ConstDeclaration.Create($1, $5, $3); $3.Directives := $4; }
    ;
    
constinit
    : constexpr                     { $$ := $1; }
    | arrayconst                    { $$ := $1; }
    | recordconst                   { $$ := $1; }
    ;

constexpr
    : expr                          { $$ := $1; $1.EnforceConst := true; }
    ;

constexprlst
    : constexpr                     { $$ := ExpressionList.Create(); $$.Add($1); }
    | constexprlst COMMA constexpr  { $$ := $1; $1.Add($3); }
    ;

arrayconst
    : LPAR constinit COMMA arrexprlst RPAR  { $$ := ArrayConst.Create($4); $4.InsertAt(0, $2); }
    ;

arrexprlst
    : constinit                             { $$ := ExpressionList.Create($1); }
    | arrexprlst COMMA constinit            { $$ := $1; $1.Add($3); }
    ;

recordconst
    : LPAR fieldconstlst scolopt RPAR       { $$ := RecordConst.Create($2); }
    ;

fieldconstlst
    : fieldconst                            { $$ := FieldInitList.Create(); $$.Add($1);}
    | fieldconstlst SCOL fieldconst         { $$ := $1; $1.Add($3); }
    ;

fieldconst
    : id COLON constinit                    { $$ := FieldInit.Create($1, $3); }
    ;
	
recordtype
    : KW_RECORD KW_END                                  { $$ := RecordType.Create(DeclarationList.Create()); }
    | KW_RECORD fieldlst scolopt KW_END                 { $$ := RecordType.Create($2); }
    | KW_RECORD fieldlst SCOL recvariant scolopt KW_END { $$ := RecordType.Create($2); }
    | KW_RECORD recvariant scolopt KW_END               { $$ := RecordType.Create($2); }
    ;
    
recvariant
    : KW_CASE id COLON ordinaltype KW_OF recfieldlst    { $$ := DeclarationList.Create(VariantDeclaration.Create($2, $4, $6)); }
    | KW_CASE          ordinaltype KW_OF recfieldlst    { $$ :=  DeclarationList.Create(VariantDeclaration.Create(Nil, $2, $4)); }
    ;

recfieldlst
    : recfield %prec LOWESTPREC                         { $$ := $1; }
    | recfield SCOL                                     { $$ := $1; }
    | recfield SCOL recfieldlst                         { $$ := $1; $1.InsertAt(0, $1); }
    ;

recfield
    : constexprlst COLON LPAR recvarlst scolopt RPAR    { $$ := CreateRecordUnionField($1, $4); }
    ;
    
recvarlst
    : recvarfield                                       { $$ := $1; }
    | recvarlst SCOL recvarfield                        { $$ := $1; $1.Add($3); }
    ;

recvarfield
    : objfield                                          { $$ := $1; }
    | recvariant                                        { $$ := $1; }
    ;
    
classtype
    : kwclass heritage classbody KW_END { $$ := ClassTypeNode.Create($2, $3); }
    | kwclass heritage                  { $$ := ClassTypeNode.Create($2); } 
    ;

heritage
    :                                   { $$ := Nil; }
    | LPAR idlst RPAR                   { $$ := $2; } 
    ;

classbody
    : class1stsec scopeseclst           { $$ := $1; $1.Add($2); }
    ;

class1stsec
    : cfieldlst ccomplstopt             { $$ := ObjectSection.Create($1, $2); }
    |           ccomplstopt             { $$ := ObjectSection.Create(Nil, $1); }
    ;
    
scopeseclst
    :                                   { $$ := ObjectSection.Create(); }
    | scopeseclst scope ccomplstopt     { $$ := $1; $1.AddDecls($3, $2); } 
    | scopeseclst scope cfieldlst
                        ccomplstopt     { $$ := $1; $1.AddDecls($4, $2); $1.AddFields($3, $2); } 
    ;
    
scope
    : KW_PUBLISHED                      { $$ := Scope_Published; }
    | KW_PUBLIC                         { $$ := Scope_Public; }
    | KW_PROTECTED                      { $$ := Scope_Protected; }
    | KW_PRIVATE                        { $$ := Scope_Private; }
    ;
    
cfieldlst
    : fieldlst SCOL                     { $$ := $1; }
    | KW_CLASS KW_VAR fieldlst SCOL     { $$ := $3; MakeFieldDeclarationsStatic($3); }
    ;
    
fieldlst
    : objfield                          { $$ := $1; }
    | fieldlst SCOL objfield            { $$ := $1; $1.Add($3); }
    ;
    
objfield
    : idlst COLON vartype               { $$ := CreateFieldDecls($1, $3); }
    | idlst COLON proceduralsign        { $$ := CreateFieldDecls($1, $3); }
    | idlst COLON proceduralsign
            SCOL funcdirectlst          { $$ := CreateFieldDecls($1, $3); $3.Directives := $5; }
    ;
    
ccomplstopt
    :                                   { $$ := DeclarationList.Create(); }
    | classcomplst                      { $$ := $1; }
    ;
    
classcomplst
    : classcomp                         { $$ := DeclarationList.Create(); $$.Add($1); }
    | classcomplst classcomp            { $$ := $1; $1.Add($2); }
    ;
    
classcomp
    : staticopt classmetdecl            { $$ := $2; $2.isStatic := $1; }
    | property                          { $$ := $1; }
    ;

staticopt
    :                                   { $$ := False; }
    | KW_CLASS                          { $$ := True ; }
    ;

interftype
    : KW_INTERF heritage guid interfbody{ $$ := InterfaceType.Create($2, $4, $3); }
    | KW_INTERF heritage                { $$ := InterfaceType.Create($2); }
    ;

interfbody
    : interfcomplst KW_END              { $$ := ObjectSection.Create(Nil, $1, Scope_Public); }
    ;
    
interfcomplst
    : interfcomp                        { $$ := DeclarationList.Create($1); }
    | interfcomplst interfcomp          { $$ := $1; $1.Add($2); }
    ;
    
interfcomp
    : interfmetdecl                     { $$ := $1; }
    | property                          { $$ := $1; }
    ;
    
guid
    :                                   { $$ := Nil; }
    | LBRAC stringlit RBRAC             { $$ := $2; }
    ;

        
property
    : KW_PROPERTY id COLON funcrettype
        spropspecsnormal SCOL                       { $$ := PropertyDeclaration.Create($2, $4, $5); }
    | KW_PROPERTY id arrayprops COLON funcrettype
        spropspecsarray SCOL defaultdiropt          { $$ := ArrayProperty.Create($2, $5, $3, $6, $8); }
    | KW_PROPERTY id spropspecsoverride SCOL        { $$ := PropertyDeclaration.Create($2, Nil, $3); }
    ;

defaultdiropt
    :                                               { $$ := false;}
    | id                                            { $$ := CheckDirectiveId('default', $1); }
    ;

arrayprops
    : LBRAC propfield RBRAC                         { $$ := $2; }
    ;

propfield
    :           idlst COLON vartype                 { $$ := CreateVarParamDecls($1, $3); }
    | KW_CONST  idlst COLON vartype                 { $$ := CreateConstParamDecls($2, $4); }
    ;


spropspecsnormal
    : indexopt readopt writeopt storeopt defaultopt { $$ := PropertySpecifiers.Create($1, $2, $3, $4, $5); }
    ;

spropspecsarray
    : readopt writeopt                              { $$ := PropertySpecifiers.Create($1, $2); }
    ;

spropspecsoverride
    : indexopt readopt writeopt storeopt defaultopt implopt { $$ := PropertySpecifiers.Create($1, $2, $3, $4, $5, $6); }
    ;
    
indexopt
    :                           { $$ := Nil; }
    | KW_INDEX constint         { $$ := IntLiteral.Create($2);  }
    ;

readopt
    :                           { $$ := Nil; }
    | KW_READ   id              { $$ := $2; }
    ;

writeopt
    :                           { $$ := Nil; }
    | KW_WRITE  id              { $$ := $2; }
    ;

storeopt
    :                           { $$ := BoolLiteral.Create(true);  }
    | KW_STORED id              { $$ := ConstIdentifier.Create($2);  }
    | KW_STORED constbool       { $$ := BoolLiteral.Create($2);  }
    ;
    
defaultopt
    :                           { $$ := IntLiteral.Create(MaxInt);  }
    | KW_NODEFAULT              { $$ := IntLiteral.Create(MaxInt);  }
    | KW_DEFAULT constexpr      { $$ := ConstExpression($2).ResolveToLiteral(); }
    ;
    
implopt
    :                           { $$ := Nil;  }
    | KW_IMPLEMENTS id          { $$ := $2; }
    ;


typedecl
    : idtypeopt vartype  SCOL       { $$ := TypeDeclaration.Create($1, $2); }
    | idtypeopt proceduraltype      { $$ := TypeDeclaration.Create($1, $2); }
    | idtypeopt packclasstype SCOL  { $$ := ClassDeclaration.Create($1,$2); $2.Name := $1; }
    | idtypeopt packinterftype SCOL { $$ := InterfaceDeclaration.Create($1, $2); $2.Name := $1; }
    ;

idtypeopt
    : id KW_EQ  typeopt             { $$ := $1; _lastObjName := $1; }
    ;

typeopt    
    :
    | KW_TYPE
    ;

vartype
    : qualifid                      { $$ := UnresolvedType.Create($1); }
    | stringtype                    { $$ := $1; }
    | KW_DEREF vartype              { $$ := PointerType.Create($2); }
    | TYPE_PTR                      { $$ := MetaType.Create(PointerType); }
    | KW_CLASS KW_OF qualifid       { $$ := MetaclassType.Create(ClassRefType.Create($3)); }
    | packstructtype                { $$ := $1; }
    | rangetype                     { $$ := $1; }
    | enumtype                      { $$ := $1; }
    ;

ordinaltype
    : rangetype                     { $$ := $1; }
    | qualifid                      { $$ := UnresolvedOrdinalType.Create($1); }
    ;

funcpartype
    : qualifid                      { $$ := UnresolvedVariableType.Create($1); }
    | TYPE_ARRAY KW_OF funcpartype  { $$ := ArrayType.Create($3); }
    | stringtype                    { $$ := $1; }
    | TYPE_PTR                      { $$ := MetaType.Create(PointerType); }
    ;

funcrettype
    : qualifid                      { $$ := UnresolvedType.Create($1); }
    | TYPE_PTR                      { $$ := MetaType.Create(PointerType); }
    | TYPE_STR                      { $$ := MetaType.Create(StringType); }
    ;
        
packclasstype
    : classtype                     { $$ := $1; }
    | KW_PACKED classtype           { $$ := $2; $2.IsPacked := true; }
    ;

packinterftype
    : interftype                    { $$ := $1; }
    | KW_PACKED interftype          { $$ := $2; $2.IsPacked := true; }
    ;
    
stringtype
    : TYPE_STR                      { $$ := MetaType.Create(StringType); }
    | TYPE_STR LBRAC constexpr RBRAC{ $$ := StringType.Create($3); }
    ;

    
packstructtype
    : structuredtype                { $$ := $1; }
    | KW_PACKED structuredtype      { $$ := $2; $2.IsPacked := true; }
    ;
    
structuredtype
    : arraytype                     { $$ := $1; }
    | settype                       { $$ := $1; }
    | filetype                      { $$ := $1; }
    | recordtype                    { $$ := $1; }
    ;
    
arrayszlst
    : rangetype                     { $$ := TypeList.Create(); $$.Add($1); }
    | arrayszlst COMMA rangetype    { $$ := $1; $$.Add($3); }
    ;

arraytype
    : TYPE_ARRAY LBRAC arrayszlst RBRAC KW_OF vartype   { $$ := ArrayType.Create($6, $3); }
    | TYPE_ARRAY LBRAC qualifid   RBRAC KW_OF vartype   { $$ := ArrayType.Create($6, UnresolvedIntegralType.Create($3)); }
    | TYPE_ARRAY KW_OF vartype      { $$ := ArrayType.Create($3); }
    ;

settype
    : TYPE_SET KW_OF ordinaltype    { $$ := SetType.Create($3);}
    ;

filetype
    : TYPE_FILE KW_OF fixedtype     { $$ := FileType.Create($3); }
    | TYPE_FILE                     { $$ := FileType.Create(Nil); }
    ;

fixedtype
    : qualifid                      { $$ := UnresolvedVariableType.Create($1); }
    | TYPE_STR LBRAC constexpr RBRAC{ $$ := StringType.Create($3); }
    | packstructtype                { $$ := $1; }
    | rangetype                     { $$ := $1; }
    | proceduraltype                { $$ := $1; }
    ;
    
qualifid    
    : id                            { $$ := $1; }
    | id KW_DOT id                  { $$ := StringObject.Create($1.Value + '.' + $3.Value); }
    ;
    
%%   

