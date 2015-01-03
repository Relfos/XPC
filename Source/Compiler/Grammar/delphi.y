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
%type <IdentifierListNode> labelidlst idlst varids heritage 

%type <SourceNode> goal file program library unit package 
%type <UnitItemNode> containsitem usesitem expitem 
%type <UnitListNode> usesopt useslst requires requireslst containslst contains 
%type <DeclarationListNode> interfdecllst interfdecl maindecllst declseclst maindeclsec funcdeclsec basicdeclsec expitemlst
%type <DeclarationListNode> thrvarsec rscstringsec exportsec labeldeclsec constsec
%type <DeclarationListNode> typesec varsec rscstringlst vardecl objfield
%type <ImplementationSectionNode> implsec
%type <InterfaceSectionNode> interfsec
%type <BlockStatementNode> initsec finalsec
%type <ConstDeclarationNode> constdecl rscstring
%type <TypeDeclarationNode> typedecl
%type <RoutineSectionNode> funcdefine

%type <CallableDeclarationNode> routinedeclmain
%type <RoutineDeclarationNode> routineproto routinedeclinterf
%type <MethodDeclarationNode> methoddecl classmetdecl interfmetdecl 
%type <MethodDefinitionNode> methoddef metdefproto
%type <RoutineDefinitionNode> routinedef
%type <FunctionDirectiveListNode> funcdirectlst funcdir_noterm_opt funcdiropt importdirforced  metdirectopt smetdirslst smetdirs metdirectlst  
%type <FunctionAttributeNode> funcdirective smetqualif metdirective metqualif funcqualif funcdeprecated routinecallconv
%type <MethodKindEnum> kwmetspec
%type <DeclarationListNode> formalparamslst formalparm
%type <ParametersSectionNode> formalparams
%type <ExternalDirectiveNode> externarg
%type <StringObject> kwfunction kwprocedure

%type <BlockStatementNode> block funcblock assemblerstmt blockstmt
%type <ExceptionBlockNode> exceptionblock
%type <StatementNode> stmt nonlblstmt assign goto_stmt ifstmt casestmt elsecase caseselector 
%type <StatementNode> tryexceptstmt tryfinallystmt raisestmt ondef repeatstmt whilestmt forstmt withstmt 
%type <StatementListNode> casesellst onlst stmtlist asmcode

%type <LiteralNode> literal basicliteral discrete strorcharlit
%type <StringLiteralNode> stringlit
%type <Integer> mulop addop relop fordir
%type <Cardinal> constint constnil
%type <Boolean>   constbool
%type <Double> constreal
%type <AnsiChar>   constchar
%type <LvalueExpressionNode> lvalue lvalstmt
%type <IdentifierNode> identifier
%type <ExpressionNode> unaryexpr expr rangestart set setelem constexpr constinit paraminitopt lvalasval
%type <ExpressionListNode> caselabellst exprlst constexprlst exprlstopt setelemlst arrexprlst callparams
%type <TypeListNode> arrayszlst
%type <FieldInitListNode> fieldconstlst 
%type <FieldInitNode> fieldconst
%type <EnumValueListNode> enumelemlst
%type <EnumValueNode> enumelem
%type <ConstExpressionNode> functypeinit arrayconst recordconst

%type <DeclarationListNode> recvariant recfield recvarfield recfieldlst propfield recvarlst 
%type <DeclarationListNode> ccomplstopt fieldlst cfieldlst classcomplst interfcomplst arrayprops
%type <DeclarationNode> classcomp property interfcomp
%type <InterfaceTypeNode> packinterftype interftype
%type <ClassTypeNode> classtype packclasstype
%type <StringLiteralNode> guid
%type <ScopeEnum> scope
%type <Boolean> staticopt defaultdiropt
%type <ObjectSectionNode> classbody interfbody class1stsec scopeseclst
%type <StringObject> implopt readopt writeopt
%type <IntLiteralNode> indexopt 
%type <LiteralNode> defaultopt 
%type <ConstExpressionNode> storeopt
%type <PropertySpecifiersNode> spropspecsnormal spropspecsoverride spropspecsarray

%type <TypeNode> paramtypeopt paramtypespec funcpartype  
%type <TypeNode> enumtype rangetype ordinaltype fixedtype stringtype
%type <TypeNode> vartype funcrettype funcret
%type <StructuredTypeNode> structuredtype packstructtype recordtype  arraytype settype filetype
%type <ProceduralTypeNode> proceduraltype proceduralsign
    

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
    : programid usesopt maindecllst block   { $$ := ProgramNode.Create($1, $2,   $3, $4); Self._Root := $$; }
    | programid usesopt             block   { $$ := ProgramNode.Create($1, $2, Nil, $3); Self._Root := $$; }
    ;
    
programid
    :                                       { $$ := Nil; }
    | KW_PROGRAM id SCOL                    { $$ := $2;   }
    ; 
    
library
    : id SCOL usesopt maindecllst block     { $$ := LibraryNode.Create($1, $3, $4  , $5); Self._Root := $$; }
    | id SCOL usesopt             block     { $$ := LibraryNode.Create($1, $3, Nil, $4); Self._Root := $$; }
    ;
    
package
    : id SCOL requires contains             { $$ := PackageNode.Create($1, $3, $4);Self ._Root := $$; }
    ;

requires
    :                                       { $$ := UnitListNode.Create(); }
    | KW_REQUIRES requireslst SCOL          { $$ := $2; }
    ;
    
requireslst
    : id                                    { $$ := UnitListNode.Create(); $$.Add(UnitItemNode.Create($1)); }
    | requireslst COMMA id                  { $1.Add(UnitItemNode.Create($3)); $$ := $1; }
    ;

contains
    : KW_CONTAINS containslst SCOL          { $$ := $2; }
    ;

containslst
    : containsitem                          { $$ := UnitListNode.Create(); $$.Add($1); }
    | containslst COMMA containsitem        { $1.Add($3); $$ := $1; }
    ;

containsitem
    : id                                    { $$ := UnitItemNode.Create($1); }
    | id KW_IN stringnonNil                { $$ := UnitItemNode.Create($1, $3); }
    ;

usesopt
    :                                       { $$ := UnitListNode.Create(); }
    | KW_USES useslst SCOL                  { $$ := $2; }
    ;

useslst
    : usesitem                              { $$ := UnitListNode.Create(); $$.Add($1); }
    | useslst COMMA usesitem                { $1.Add($3); $$ := $1; }
    ;
    
usesitem
    : id                                    { $$ := UnitItemNode.Create($1); }
    | id KW_IN stringnonNil                { $$ := UnitItemNode.Create($1, $3);}
    ;

unit
    : id SCOL interfsec implsec initsec finalsec  { $$ := UnitNode.Create($1, $3, $4, $5, $6); Self._Root := $$; }
    | id SCOL interfsec implsec initsec     { $$ := UnitNode.Create($1, $3, $4, $5, Nil); Self._Root := $$; }
    | id SCOL interfsec implsec finalsec    { $$ := UnitNode.Create($1, $3, $4, Nil, $5); Self._Root := $$; }
    | id SCOL interfsec implsec             { $$ := UnitNode.Create($1, $3, $4); Self._Root := $$; }
    ;

implsec
    : KW_IMPL usesopt   maindecllst         { $$ := ImplementationSectionNode.Create($2, $3);}
    | KW_IMPL usesopt                       { $$ := ImplementationSectionNode.Create($2, Nil);}
    ;

interfsec
    : KW_INTERF usesopt interfdecllst       { $$ := InterfaceSectionNode.Create($2, $3); }
    ;

interfdecllst
    :                                       { $$ := DeclarationListNode.Create();}
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
    | routinedeclinterf     { $$ := DeclarationListNode.Create(); $$.Add($1);}
    | thrvarsec             { $$ := $1;}
    | rscstringsec          { $$ := $1;}
    ;

maindeclsec
    : basicdeclsec          { $$ := $1;}
    | thrvarsec             { $$ := $1;}
    | exportsec             { $$ := $1;}
    | routinedeclmain       { $$ := DeclarationListNode.Create(); $$.Add($1);}
    | labeldeclsec          { $$ := $1;}
    ;

funcdeclsec
    : basicdeclsec          { $$ := $1;}
    | labeldeclsec          { $$ := $1;}
    | routinedef            { $$ := DeclarationListNode.Create(); $$.Add($1);}
    ;

basicdeclsec
    : constsec              { $$ := $1;}
    | typesec               { $$ := $1;}
    | varsec                { $$ := $1;}
    ;

typesec
    : KW_TYPE typedecl      { $$ := DeclarationListNode.Create(); $$.Add($2); }
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
    : rscstring                     { $$ := DeclarationListNode.Create(); $$.Add($1); }
    | rscstringlst rscstring        { $1.Add($2); $$ := $1; }
    ;
    
rscstring
    :  id KW_EQ stringlit SCOL      { $$ := ConstDeclarationNode.Create($1, $3); }
    ;

    
labeldeclsec
    : KW_LABEL labelidlst SCOL      { $$ := CreateLabelDecls($2); }
    ;
    
labelidlst 
    : labelid                       { $$ := IdentifierListNode.Create(IdentifierNode.Create($1)); }
    | labelidlst COMMA labelid      { $$ := $1; $1.Add(IdentifierNode.Create($3)); }
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
    : expitem                       { $$ := DeclarationListNode.Create($1); }
    | expitemlst COMMA expitem      { $$ := $1; $1.Add($3); }
    ;

expitem
    : expid formalparams                        { $$ := ExportItemNode.Create($1, $2); }
    | expid formalparams KW_NAME stringnonNil  { $$ := ExportItemNode.Create($1, $2, $4); }
    | expid formalparams KW_INDEX constint      { $$ := ExportItemNode.Create($1, $2, $4); }
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
    : routineproto funcdiropt funcdefine SCOL   { $$ := RoutineDefinitionNode.Create($1.name, $1.SignatureType, $2, $3); }
    ;

methoddecl
    : kwfunction  formalparams funcret SCOL     { $$ := MethodDeclarationNode.Create(_lastObjName, $1, $2, $3); }
    | kwprocedure formalparams         SCOL     { $$ := MethodDeclarationNode.Create(_lastObjName, $1, $2, Nil); }
    ;
    
routineproto
    : kwfunction  formalparams funcret  SCOL    { $$ := RoutineDeclarationNode.Create($1, $2, $3); }
    | kwprocedure formalparams          SCOL    { $$ := RoutineDeclarationNode.Create($1, $2); }
    ;   

methoddef
    :           metdefproto SCOL metdirectopt   { $$ := $1; $1.Directives := $3; }
    | KW_CLASS  metdefproto SCOL metdirectopt   { $$ := $2; $2.Directives := $4; $2.isStatic := true; }
    | kwmetspec id KW_DOT id formalparams SCOL smetdirs { $$ := MethodDefinitionNode.Create($2, $4, $5, Nil, $7, $1);}
    ;

metdefproto
    : kwfunction KW_DOT id formalparams funcret { $$ := MethodDefinitionNode.Create($1, $3, $4, $5); }
    | kwprocedure KW_DOT id formalparams        { $$ := MethodDefinitionNode.Create($1, $3, $4); }
    ;
    
routinedeclinterf
    : routineproto                              { $$ := $1; }
    ;

classmetdecl
    : methoddecl metdirectopt                   { $$ := $1; $1.Directives := $2; }
    | kwmetspec id formalparams SCOL smetdirs   { $$ := MethodDeclarationNode.Create(_lastObjName, $2, $3, Nil, $5, $1); }
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
    : KW_PROCEDURE formalparams                 { $$ := ProceduralTypeNode.Create($2); }
    | KW_FUNCTION  formalparams funcret         { $$ := ProceduralTypeNode.Create($2, $3); } 
    | KW_PROCEDURE formalparams typeobj         { $$ := MethodTypeNode.Create($2); } 
    | KW_FUNCTION  formalparams funcret typeobj { $$ := MethodTypeNode.Create($2, $3); } 
    ;

typeobj
    : KW_OF KW_OBJECT
    ;
    
funcret
    : COLON funcrettype                         { $$ := $2;}
    ;


funcdefine
    : declseclst funcblock                      { $$ := RoutineSectionNode.Create($1, $2); }
    |            funcblock                      { $$ := RoutineSectionNode.Create(Nil, $1); }
    ;

funcblock
    : block                                     { $$ := $1; }
    | assemblerstmt                             { $$ := $1; }
    ;

formalparams
    :                                           { $$ := ParametersSectionNode.Create(); }
    | LPAR RPAR                                 { $$ := ParametersSectionNode.Create(); }
    | LPAR formalparamslst RPAR                 { $$ := ParametersSectionNode.Create($2); }
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
    : KW_EQ id                                  { $$ := ConstIdentifierNode.Create($2); }
    | KW_EQ constnil                            { $$ := NullLiteralNode.Create(); }
    ;

    
importdirforced
    : funcdiropt KW_EXTERNAL externarg SCOL funcdiropt  { $$ := JoinImportDirectives($1, $5, $3); }
    | funcdiropt KW_FORWARD SCOL funcdiropt             { $$ := JoinImportDirectives($1, $4, FunctionAttributeNode.Create(attribute_Forward)); }
    ;

externarg
    : constexpr KW_NAME constexpr       { $$ := ExternalDirectiveNode.Create($1, $3); }
    | constexpr                         { $$ := ExternalDirectiveNode.Create($1); }
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
    : funcdirective                     { $$ := FunctionDirectiveListNode.Create($1); }
    | funcdirectlst SCOL funcdirective  { $$ := $1; $1.Add($3); }
    ;

smetdirslst
    : smetqualif                        { $$ := FunctionDirectiveListNode.Create($1); }
    | smetdirslst SCOL smetqualif       { $$ := $1; $1.Add($3); }
    ;

metdirectlst
    : metdirective                      { $$ := FunctionDirectiveListNode.Create($1); }
    | metdirectlst SCOL metdirective    { $$ := $1; $1.Add($3); }
    ;

smetqualif
    : KW_OVERLOAD           { $$ := FunctionAttributeNode.Create(attribute_Overload); }
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
    : KW_FAR                { $$ := FunctionAttributeNode.Create(attribute_Far); }
    | KW_NEAR               { $$ := FunctionAttributeNode.Create(attribute_Near); }
    | KW_RESIDENT           { $$ := FunctionAttributeNode.Create(attribute_Resident); }
    ;

metqualif
    : KW_ABSTRACT           { $$ := FunctionAttributeNode.Create(attribute_Abstract); }
    | KW_DYNAMIC            { $$ := FunctionAttributeNode.Create(attribute_Dynamic); }
    | KW_OVERRIDE           { $$ := FunctionAttributeNode.Create(attribute_Override); }
    | KW_VIRTUAL            { $$ := FunctionAttributeNode.Create(attribute_Virtual); }
    | KW_REINTRODUCE        { $$ := FunctionAttributeNode.Create(attribute_Reintroduce); }
    ;

funcqualif
    : KW_ASSEMBLER          { $$ := FunctionAttributeNode.Create(attribute_Assembler); }
    | KW_EXPORT             { $$ := FunctionAttributeNode.Create(attribute_Export); }
    | KW_INLINE             { $$ := FunctionAttributeNode.Create(attribute_Inline); }
    | KW_OVERLOAD           { $$ := FunctionAttributeNode.Create(attribute_Overload); }
    | KW_VARARGS            { $$ := FunctionAttributeNode.Create(attribute_VarArgs); }
    ;
    
routinecallconv
    : KW_PASCAL             { $$ := FunctionAttributeNode.Create(attribute_call_Pascal); }
    | KW_SAFECALL           { $$ := FunctionAttributeNode.Create(attribute_call_SafeCall); }
    | KW_STDCALL            { $$ := FunctionAttributeNode.Create(attribute_call_StdCall); }
    | KW_CDECL              { $$ := FunctionAttributeNode.Create(attribute_call_CDecl); }
    | KW_REGISTER           { $$ := FunctionAttributeNode.Create(attribute_call_Register); }
    ;
    

block
    : KW_BEGIN blockstmt KW_END         { $$ := $2; }
    ;

blockstmt
    : stmtlist                          { $$ := BlockStatementNode.Create(StatementListNode.Create($1)); }
    ;
    
stmtlist
    : stmt                              { $$ := StatementListNode.Create($1); }
    | stmt SCOL stmtlist                { $$ := $3; $3.Add($1); }
    ;

stmt
    : nonlblstmt                        { $$ := $1; }
    | labelid COLON nonlblstmt          { $$ := LabelStatementNode.Create($1, $3); }
    ;

nonlblstmt
    :                                   { $$ := EmptyStatementNode.Create(); }
    | lvalstmt                          { $$ := ExpressionStatementNode.Create($1); }
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
    | KW_BREAK                          { $$ := BreakStatementNode.Create(); }
    | KW_CONTINUE                       { $$ := ContinueStatementNode.Create(); }
    ;

assign
    : lvalue KW_ASSIGN expr             { $$ := AssignmentNode.Create($1, $3); }
    ;

goto_stmt
    : KW_GOTO labelid                   { $$ := GotoStatementNode.Create($2); }
    ;

ifstmt
    : KW_IF expr KW_THEN nonlblstmt KW_ELSE nonlblstmt  { $$ := IfStatementNode.Create($2, $4, $6); }
    | KW_IF expr KW_THEN nonlblstmt                     { $$ := IfStatementNode.Create($2, $4, Nil); }
    ;

casestmt
    : KW_CASE expr KW_OF casesellst
        elsecase KW_END                 { $$ := CaseStatementNode.Create($2, $4, $5); }
    ;

elsecase
    :                                   { $$ := Nil;}   
    | KW_ELSE nonlblstmt                { $$ := $2; }
    | KW_ELSE nonlblstmt SCOL           { $$ := $2; }
    ;

casesellst
    : caseselector                      { $$ := StatementListNode.Create($1); }
    | casesellst SCOL caseselector      { $$ := $1; $1.Add($3); }
    ;

caseselector
    :                                   { $$ := Nil; }
    | caselabellst COLON nonlblstmt     { $$ := CaseSelectorNode.Create($1, $3); }
    ;
    
caselabellst
    : setelem                           { $$ := ExpressionListNode.Create($1); }
    | caselabellst COMMA setelem        { $$ := $1; $1.Add($3); }
    ;

repeatstmt
    : KW_REPEAT blockstmt KW_UNTIL expr { $$ := RepeatLoopNode.Create($2, $4); }
    ;

whilestmt
    : KW_WHILE expr KW_DO nonlblstmt    { $$ := WhileLoopNode.Create($4, $2); }
    ;

forstmt
    : KW_FOR identifier KW_ASSIGN expr
        fordir expr KW_DO nonlblstmt    { $$ := ForLoopNode.Create($2, $4, $6, $8, $5); }
    ;

fordir
    : KW_TO                             { $$ := 1; }
    | KW_DOWNTO                         { $$ := -1; }
    ;

withstmt
    : KW_WITH exprlst KW_DO nonlblstmt  { $$ := WithStatementNode.Create($2, $4); }
    ;

tryexceptstmt
    : KW_TRY blockstmt KW_EXCEPT
        exceptionblock KW_END           { $$ := TryExceptStatementNode.Create($2, $4); }
    ;

exceptionblock
    : onlst KW_ELSE blockstmt           { $$ := ExceptionBlockNode.Create($1, $3); }
    | onlst                             { $$ := ExceptionBlockNode.Create($1); }
    | blockstmt                         { $$ := ExceptionBlockNode.Create(Nil, $1); }
    ;

onlst
    : ondef                             { $$ := StatementListNode.Create($1); }
    | onlst ondef                       { $$ := $1; $1.Add($2); }
    ;

ondef
    : KW_ON id COLON id KW_DO nonlblstmt SCOL   { $$ := OnStatementNode.Create($2, $4, $6); }
    | KW_ON          id KW_DO nonlblstmt SCOL   { $$ := OnStatementNode.Create(Nil, $2, $4); }
    ;

tryfinallystmt
    : KW_TRY blockstmt 
        KW_FINALLY blockstmt KW_END     { $$ := TryFinallyStatementNode.Create($2, $4); }
    ;

raisestmt
    : KW_RAISE                          { $$ := RaiseStatementNode.Create(Nil, Nil); }
    | KW_RAISE lvalue                   { $$ := RaiseStatementNode.Create($2, Nil); }
    | KW_RAISE          KW_AT expr      { $$ := RaiseStatementNode.Create(Nil, $3); }
    | KW_RAISE lvalue   KW_AT expr      { $$ := RaiseStatementNode.Create($2, $4); }
    ;

assemblerstmt
    : KW_ASM asmcode KW_END             { $$ := AssemblerBlockNode.Create($2); }
    ;

asmcode
    :                                   { $$ := Nil; }
    | asmcode ASM_OP                    { $$ := Nil; }
    ;

identifier
    : id                                { $$ := IdentifierNode.Create($1); }
    ;
    
lvalstmt
    : identifier                        { $$ := UnresolvedIdNode.Create($1); }
    | lvalue LPAR exprlstopt RPAR       { $$ := UnresolvedCallNode.Create($1, $3); }
    | TYPE_PTR LPAR expr RPAR           { $$ := StaticCastNode.Create(PointerTypeNode, $3); }
    | lvalue KW_DOT id                  { $$ := ObjectAccessNode.Create($1, $3); }
    | KW_INHERITED                      { $$ := InheritedCallNode.Create(Nil); }
    | KW_INHERITED id callparams        { $$ := InheritedCallNode.Create($2, $3); }
    ;
    
lvalue 
    : identifier                        { $$ := UnresolvedIdNode.Create($1); }
    | lvalue LPAR exprlstopt RPAR       { $$ := UnresolvedCallNode.Create($1, $3); }
    | TYPE_PTR LPAR expr RPAR           { $$ := StaticCastNode.Create(PointerTypeNode, $3); }
    | lvalue KW_DOT id                  { $$ := ObjectAccessNode.Create($1, $3); }
    | lvalue KW_DEREF                   { $$ := PointerDereferenceNode.Create($1); }
    | lvalue LBRAC exprlst RBRAC        { $$ := ArrayAccessNode.Create($1, $3); }
    | LPAR expr RPAR                    { $$ := ExprAsLvalueNode.Create($2); }
    ;

lvalasval
    : lvalue                            { If ($1 Is ExprAsLvalueNode) Then $$ := ($1 as ExprAsLvalueNode).expr Else $$ := LvalueAsExprNode.Create($1); }
    ;

unaryexpr
    : literal                           { $$ := $1; }
    | lvalasval                         { $$ := $1; } 
    | set                               { $$ := $1; }
    | KW_ADDR lvalue                    { $$ := AddressLvalueNode.Create($2); }
    | KW_NOT unaryexpr                  { $$ := LogicalNotNode.Create($2); }
    | KW_SUM unaryexpr                  { $$ := UnaryPlusNode.Create($2); }
    | KW_SUB unaryexpr                  { $$ := UnaryMinusNode.Create($2); }
    | KW_INHERITED                      { $$ := InheritedCallNode.Create(Nil); }
    | KW_INHERITED id callparams        { $$ := InheritedCallNode.Create($2, $3); }
    | stringnonNil LBRAC expr RBRAC    { $$ := ArrayAccessNode.Create(ArrayConstNode.Create($1), ExpressionListNode.Create($3)); }
    ;

callparams
    :                                   { $$ := ExpressionListNode.Create(); }
    | LPAR exprlstopt RPAR              { $$ := $2; }
    ;

expr
    : unaryexpr                         { $$ := $1; }
    | expr KW_AS qualifid               { $$ := RuntimeCastNode.Create($1, ClassRefTypeNode.Create($3)); }
    | expr KW_IS qualifid               { $$ := IsExpressionNode.Create($1, ClassRefTypeNode.Create($3)); }
    | expr KW_IN expr                   { $$ := InExpressionNode.Create($1, $3); }
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
    : expr          { $$ := ExpressionListNode.Create($1); }
    | exprlst COMMA expr  { $$ := $1; $1.Add($3); }
    ;

exprlstopt
    :               { $$ := ExpressionListNode.Create(); }
    | exprlst       { $$ := $1; }
    ;
   
literal
    : basicliteral  { $$ := $1; }
    | strorcharlit  { $$ := $1; }
    ;

basicliteral
    : constint      { $$ := IntLiteralNode.Create($1);}
    | constbool     { $$ := BoolLiteralNode.Create($1);}
    | constreal     { $$ := RealLiteralNode.Create($1);}
    | constnil      { $$ := NullLiteralNode.Create();}
    ;

discrete
    : constint      { $$ := IntLiteralNode.Create($1);}
    | constchar     { $$ := CharLiteralNode.Create($1);}
    | constbool     { $$ := BoolLiteralNode.Create($1);}
    ;

strorcharlit
    : stringorchar  { If (Length($1.Value)=1) Then $$ := CharLiteralNode.Create($1.Value[1]) Else $$ := StringLiteralNode.Create($1); }
    ;

stringlit
    : stringconst   { $$ := StringLiteralNode.Create($1); }
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
    : id            { $$ := IdentifierListNode.Create(IdentifierNode.Create($1)); }
    | idlst COMMA id{ $$ := $1; $1.Add(IdentifierNode.Create($3)); }
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
    : rangestart KW_RANGE expr      { $$ := RangeTypeNode.Create($1, $3); $1.EnforceConst := True; $3.EnforceConst := True; }
    ;
    
rangestart
    : discrete                      { $$ := $1; }
    | KW_SUM unaryexpr              { $$ := UnaryPlusNode.Create($2); }
    | KW_SUB unaryexpr              { $$ := UnaryMinusNode.Create($2); }
    ;

enumtype
    : LPAR enumelemlst RPAR         { $$ := EnumTypeNode.Create($2); }
    ;

enumelemlst
    : enumelem                      { $$ := EnumValueListNode.Create($1); }
    | enumelemlst COMMA enumelem    { $$ := $1; $1.Add($3); }
    ;

enumelem
    : id                            { $$ := EnumValueNode.Create($1); }
    | id KW_EQ expr                 { $$ := EnumValueNode.Create($1, $3); }
    ;

set
    : LBRAC RBRAC                   { $$ := SetExpressionNode.Create();}
    | LBRAC setelemlst  RBRAC       { $$ := SetExpressionNode.Create($2); }
    ;

setelemlst
    : setelem                       { $$ := ExpressionListNode.Create(); $$.Add($1); }
    | setelemlst COMMA setelem      { $$ := $1; $1.Add($3); }
    ;
    
setelem
    : constexpr                     { $$ := $1; }
    | constexpr KW_RANGE constexpr  { $$ := SetRangeNode.Create(RangeTypeNode.Create($1, $3)); }
    ;


constsec
    : KW_CONST constdecl            { $$ := DeclarationListNode.Create(); $$.Add($2); }
    | constsec constdecl            { $$ := $1; $1.Add($2); }
    ;

constdecl
    : id KW_EQ constexpr SCOL                   { $$ := ConstDeclarationNode.Create($1, $3); }  
    | id COLON vartype KW_EQ constinit SCOL     { $$ := ConstDeclarationNode.Create($1, $5, $3); }
    | id COLON proceduralsign
        funcdir_noterm_opt functypeinit SCOL    { $$ := ConstDeclarationNode.Create($1, $5, $3); $3.Directives := $4; }
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
    : constexpr                     { $$ := ExpressionListNode.Create(); $$.Add($1); }
    | constexprlst COMMA constexpr  { $$ := $1; $1.Add($3); }
    ;

arrayconst
    : LPAR constinit COMMA arrexprlst RPAR  { $$ := ArrayConstNode.Create($4); $4.InsertAt(0, $2); }
    ;

arrexprlst
    : constinit                             { $$ := ExpressionListNode.Create($1); }
    | arrexprlst COMMA constinit            { $$ := $1; $1.Add($3); }
    ;

recordconst
    : LPAR fieldconstlst scolopt RPAR       { $$ := RecordConstNode.Create($2); }
    ;

fieldconstlst
    : fieldconst                            { $$ := FieldInitListNode.Create(); $$.Add($1);}
    | fieldconstlst SCOL fieldconst         { $$ := $1; $1.Add($3); }
    ;

fieldconst
    : id COLON constinit                    { $$ := FieldInitNode.Create($1, $3); }
    ;
	
recordtype
    : KW_RECORD KW_END                                  { $$ := RecordTypeNode.Create(DeclarationListNode.Create()); }
    | KW_RECORD fieldlst scolopt KW_END                 { $$ := RecordTypeNode.Create($2); }
    | KW_RECORD fieldlst SCOL recvariant scolopt KW_END { $$ := RecordTypeNode.Create($2); }
    | KW_RECORD recvariant scolopt KW_END               { $$ := RecordTypeNode.Create($2); }
    ;
    
recvariant
    : KW_CASE id COLON ordinaltype KW_OF recfieldlst    { $$ := DeclarationListNode.Create(VariantDeclarationNode.Create($2, $4, $6)); }
    | KW_CASE          ordinaltype KW_OF recfieldlst    { $$ :=  DeclarationListNode.Create(VariantDeclarationNode.Create(Nil, $2, $4)); }
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
    : cfieldlst ccomplstopt             { $$ := ObjectSectionNode.Create($1, $2); }
    |           ccomplstopt             { $$ := ObjectSectionNode.Create(Nil, $1); }
    ;
    
scopeseclst
    :                                   { $$ := ObjectSectionNode.Create(); }
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
    :                                   { $$ := DeclarationListNode.Create(); }
    | classcomplst                      { $$ := $1; }
    ;
    
classcomplst
    : classcomp                         { $$ := DeclarationListNode.Create(); $$.Add($1); }
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
    : KW_INTERF heritage guid interfbody{ $$ := InterfaceTypeNode.Create($2, $4, $3); }
    | KW_INTERF heritage                { $$ := InterfaceTypeNode.Create($2); }
    ;

interfbody
    : interfcomplst KW_END              { $$ := ObjectSectionNode.Create(Nil, $1, Scope_Public); }
    ;
    
interfcomplst
    : interfcomp                        { $$ := DeclarationListNode.Create($1); }
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
        spropspecsnormal SCOL                       { $$ := PropertyDeclarationNode.Create($2, $4, $5); }
    | KW_PROPERTY id arrayprops COLON funcrettype
        spropspecsarray SCOL defaultdiropt          { $$ := ArrayPropertyNode.Create($2, $5, $3, $6, $8); }
    | KW_PROPERTY id spropspecsoverride SCOL        { $$ := PropertyDeclarationNode.Create($2, Nil, $3); }
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
    : indexopt readopt writeopt storeopt defaultopt { $$ := PropertySpecifiersNode.Create($1, $2, $3, $4, $5); }
    ;

spropspecsarray
    : readopt writeopt                              { $$ := PropertySpecifiersNode.Create($1, $2); }
    ;

spropspecsoverride
    : indexopt readopt writeopt storeopt defaultopt implopt { $$ := PropertySpecifiersNode.Create($1, $2, $3, $4, $5, $6); }
    ;
    
indexopt
    :                           { $$ := Nil; }
    | KW_INDEX constint         { $$ := IntLiteralNode.Create($2);  }
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
    :                           { $$ := BoolLiteralNode.Create(true);  }
    | KW_STORED id              { $$ := ConstIdentifierNode.Create($2);  }
    | KW_STORED constbool       { $$ := BoolLiteralNode.Create($2);  }
    ;
    
defaultopt
    :                           { $$ := IntLiteralNode.Create(MaxInt);  }
    | KW_NODEFAULT              { $$ := IntLiteralNode.Create(MaxInt);  }
    | KW_DEFAULT constexpr      { $$ := ConstExpressionNode($2).ResolveToLiteral(); }
    ;
    
implopt
    :                           { $$ := Nil;  }
    | KW_IMPLEMENTS id          { $$ := $2; }
    ;


typedecl
    : idtypeopt vartype  SCOL       { $$ := TypeDeclarationNode.Create($1, $2); }
    | idtypeopt proceduraltype      { $$ := TypeDeclarationNode.Create($1, $2); }
    | idtypeopt packclasstype SCOL  { $$ := ClassDeclarationNode.Create($1,$2); $2.Name := $1; }
    | idtypeopt packinterftype SCOL { $$ := InterfaceDeclarationNode.Create($1, $2); $2.Name := $1; }
    ;

idtypeopt
    : id KW_EQ  typeopt             { $$ := $1; _lastObjName := $1; }
    ;

typeopt    
    :
    | KW_TYPE
    ;

vartype
    : qualifid                      { $$ := UnresolvedTypeNode.Create($1); }
    | stringtype                    { $$ := $1; }
    | KW_DEREF vartype              { $$ := PointerTypeNode.Create($2); }
    | TYPE_PTR                      { $$ := MetaTypeNode.Create(PointerTypeNode); }
    | KW_CLASS KW_OF qualifid       { $$ := MetaclassTypeNode.Create(ClassRefTypeNode.Create($3)); }
    | packstructtype                { $$ := $1; }
    | rangetype                     { $$ := $1; }
    | enumtype                      { $$ := $1; }
    ;

ordinaltype
    : rangetype                     { $$ := $1; }
    | qualifid                      { $$ := UnresolvedOrdinalTypeNode.Create($1); }
    ;

funcpartype
    : qualifid                      { $$ := UnresolvedVariableTypeNode.Create($1); }
    | TYPE_ARRAY KW_OF funcpartype  { $$ := ArrayTypeNode.Create($3); }
    | stringtype                    { $$ := $1; }
    | TYPE_PTR                      { $$ := MetaTypeNode.Create(PointerTypeNode); }
    ;

funcrettype
    : qualifid                      { $$ := UnresolvedTypeNode.Create($1); }
    | TYPE_PTR                      { $$ := MetaTypeNode.Create(PointerTypeNode); }
    | TYPE_STR                      { $$ := MetaTypeNode.Create(StringTypeNode); }
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
    : TYPE_STR                      { $$ := MetaTypeNode.Create(StringTypeNode); }
    | TYPE_STR LBRAC constexpr RBRAC{ $$ := StringTypeNode.Create($3); }
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
    : rangetype                     { $$ := TypeListNode.Create(); $$.Add($1); }
    | arrayszlst COMMA rangetype    { $$ := $1; $$.Add($3); }
    ;

arraytype
    : TYPE_ARRAY LBRAC arrayszlst RBRAC KW_OF vartype   { $$ := ArrayTypeNode.Create($6, $3); }
    | TYPE_ARRAY LBRAC qualifid   RBRAC KW_OF vartype   { $$ := ArrayTypeNode.Create($6, UnresolvedIntegralTypeNode.Create($3)); }
    | TYPE_ARRAY KW_OF vartype      { $$ := ArrayTypeNode.Create($3); }
    ;

settype
    : TYPE_SET KW_OF ordinaltype    { $$ := SetTypeNode.Create($3);}
    ;

filetype
    : TYPE_FILE KW_OF fixedtype     { $$ := FileTypeNode.Create($3); }
    | TYPE_FILE                     { $$ := FileTypeNode.Create(Nil); }
    ;

fixedtype
    : qualifid                      { $$ := UnresolvedVariableTypeNode.Create($1); }
    | TYPE_STR LBRAC constexpr RBRAC{ $$ := StringTypeNode.Create($3); }
    | packstructtype                { $$ := $1; }
    | rangetype                     { $$ := $1; }
    | proceduraltype                { $$ := $1; }
    ;
    
qualifid    
    : id                            { $$ := $1; }
    | id KW_DOT id                  { $$ := StringObject.Create($1.Value + '.' + $3.Value); }
    ;
    
%%   

