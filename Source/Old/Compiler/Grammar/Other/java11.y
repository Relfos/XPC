%{
/*------------------------------------------------------------------
 * Copyright (C)
 *   1996, 1997, 1998 Dmitri Bronnikov, All rights reserved.
 *
 * THIS GRAMMAR IS PROVIDED "AS IS" WITHOUT  ANY  EXPRESS  OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
 * PURPOSE, OR NON-INFRINGMENT.
 *
 * Bronikov@inreach.com
 *
 *------------------------------------------------------------------
 *
 * VERSION 1.06 DATE 20 AUG 1998
 *
 *------------------------------------------------------------------
 *
 * UPDATES
 *
 * 1.06 Correction of Java 1.1 syntax
 * 1.05 Yet more Java 1.1
 *      <qualified name>.<allocation expression>
 * 1.04 More Java 1.1 features:
 *      <class name>.this
 *      <type name>.class
 * 1.03 Added Java 1.1 features:
 *      inner classes,
 *      anonymous classes,
 *      non-static initializer blocks,
 *      array initialization by new operator
 * 1.02 Corrected cast expression syntax
 * 1.01 All shift/reduce conflicts, except dangling else, resolved
 *
 *------------------------------------------------------------------
 *
 * PARSING CONFLICTS RESOLVED
 *
 * Some Shift/Reduce conflicts have been resolved at the expense of
 * the grammar defines a superset of the language. The following
 * actions have to be performed to complete program syntax checking:
 *
 * 1) Check that modifiers applied to a class, interface, field,
 *    or constructor are allowed in respectively a class, inteface,
 *    field or constructor declaration. For example, a class
 *    declaration should not allow other modifiers than abstract,
 *    final and public.
 *
 * 2) For an expression statement, check it is either increment, or
 *    decrement, or assignment expression.
 *
 * 3) Check that type expression in a cast operator indicates a type.
 *    Some of the compilers that I have tested will allow simultaneous
 *    use of identically named type and variable in the same scope
 *    depending on context.
 *
 * 4) Change lexical definition to change '[' optionally followed by
 *    any number of white-space characters immediately followed by ']'
 *    to OP_DIM token. I defined this token as [\[]{white_space}*[\]]
 *    in the lexer.
 *
 *------------------------------------------------------------------
 *
 * UNRESOLVED SHIFT/REDUCE CONFLICTS
 *
 * Dangling else in if-then-else
 *
 *------------------------------------------------------------------
 */
%}

%token ABSTRACT
%token BOOLEAN BREAK BYTE BYVALUE
%token CASE CAST CATCH CHAR CLASS CONST CONTINUE
%token DEFAULT DO DOUBLE
%token ELSE EXTENDS
%token FINAL FINALLY FLOAT FOR FUTURE
%token GENERIC GOTO
%token IF IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE
%token LONG
%token NATIVE NEW JNULL
%token OPERATOR OUTER
%token PACKAGE PRIVATE PROTECTED PUBLIC
%token REST RETURN
%token SHORT STATIC SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY
%token VAR VOID VOLATILE
%token WHILE
%token OP_INC OP_DEC
%token OP_SHL OP_SHR OP_SHRR
%token OP_GE OP_LE OP_EQ OP_NE
%token OP_LAND OP_LOR
%token OP_DIM
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
%token ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
%token IDENTIFIER LITERAL BOOLLIT

%start CompilationUnit

%%

TypeSpecifier
	: TypeName
	| TypeName Dims
	;

TypeName
	: PrimitiveType
	| QualifiedName
	;

ClassNameList
        : QualifiedName
        | ClassNameList ',' QualifiedName
	;

PrimitiveType
	: BOOLEAN
	| CHAR
	| BYTE
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| VOID
	;

SemiColons
	: ';'
        | SemiColons ';'
        ;

CompilationUnit
	: ProgramFile
        ;

ProgramFile
	: PackageStatement ImportStatements TypeDeclarations
	| PackageStatement ImportStatements
	| PackageStatement                  TypeDeclarations
	|                  ImportStatements TypeDeclarations
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE QualifiedName SemiColons
	;

TypeDeclarations
	: TypeDeclarationOptSemi
	| TypeDeclarations TypeDeclarationOptSemi
	;

TypeDeclarationOptSemi
        : TypeDeclaration
        | TypeDeclaration SemiColons
        ;

ImportStatements
	: ImportStatement
	| ImportStatements ImportStatement
	;

ImportStatement
	: IMPORT QualifiedName SemiColons
	| IMPORT QualifiedName '.' '*' SemiColons
	;

QualifiedName
	: IDENTIFIER
	| QualifiedName '.' IDENTIFIER
	;

TypeDeclaration
	: ClassHeader '{' FieldDeclarations '}'
	| ClassHeader '{' '}'
	;

ClassHeader
	: Modifiers ClassWord IDENTIFIER Extends Interfaces
	| Modifiers ClassWord IDENTIFIER Extends
	| Modifiers ClassWord IDENTIFIER       Interfaces
	|           ClassWord IDENTIFIER Extends Interfaces
	| Modifiers ClassWord IDENTIFIER
	|           ClassWord IDENTIFIER Extends
	|           ClassWord IDENTIFIER       Interfaces
	|           ClassWord IDENTIFIER
	;

Modifiers
	: Modifier
	| Modifiers Modifier
	;

Modifier
	: ABSTRACT
	| FINAL
	| PUBLIC
	| PROTECTED
	| PRIVATE
	| STATIC
	| TRANSIENT
	| VOLATILE
	| NATIVE
	| SYNCHRONIZED
	;

ClassWord
	: CLASS
	| INTERFACE
	;

Interfaces
	: IMPLEMENTS ClassNameList
	;

FieldDeclarations
	: FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
	;

FieldDeclarationOptSemi
        : FieldDeclaration
        | FieldDeclaration SemiColons
        ;

FieldDeclaration
	: FieldVariableDeclaration ';'
	| MethodDeclaration
	| ConstructorDeclaration
	| StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
	;

FieldVariableDeclaration
	: Modifiers TypeSpecifier VariableDeclarators
	|           TypeSpecifier VariableDeclarators
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators ',' VariableDeclarator
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName '=' VariableInitializer
	;

VariableInitializer
	: Expression
	| '{' '}'
        | '{' ArrayInitializers '}'
        ;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers ',' VariableInitializer
	| ArrayInitializers ','
	;

MethodDeclaration
	: Modifiers TypeSpecifier MethodDeclarator Throws MethodBody
	| Modifiers TypeSpecifier MethodDeclarator        MethodBody
	|           TypeSpecifier MethodDeclarator Throws MethodBody
	|           TypeSpecifier MethodDeclarator        MethodBody
	;

MethodDeclarator
	: DeclaratorName '(' ParameterList ')'
	| DeclaratorName '(' ')'
	| MethodDeclarator OP_DIM
	;

ParameterList
	: Parameter
	| ParameterList ',' Parameter
	;

Parameter
	: TypeSpecifier DeclaratorName
        | FINAL TypeSpecifier DeclaratorName
	;

DeclaratorName
	: IDENTIFIER
        | DeclaratorName OP_DIM
        ;

Throws
	: THROWS ClassNameList
	;

MethodBody
	: Block
	| ';'
	;

ConstructorDeclaration
	: Modifiers ConstructorDeclarator Throws Block
	| Modifiers ConstructorDeclarator        Block
	|           ConstructorDeclarator Throws Block
	|           ConstructorDeclarator        Block
	;

ConstructorDeclarator
	: IDENTIFIER '(' ParameterList ')'
	| IDENTIFIER '(' ')'
	;

StaticInitializer
	: STATIC Block
	;

NonStaticInitializer
        : Block
        ;

Extends
	: EXTENDS TypeName
	| Extends ',' TypeName
	;

Block
	: '{' LocalVariableDeclarationsAndStatements '}'
	| '{' '}'
        ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: TypeSpecifier VariableDeclarators ';'
        | FINAL TypeSpecifier VariableDeclarators ';'
	;

Statement
	: EmptyStatement
	| LabelStatement
	| ExpressionStatement ';'
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| GuardingStatement
	| Block
	;

EmptyStatement
	: ';'
        ;

LabelStatement
	: IDENTIFIER ':'
        | CASE ConstantExpression ':'
	| DEFAULT ':'
        ;

ExpressionStatement
	: Expression
	;

SelectionStatement
	: IF '(' Expression ')' Statement
        | IF '(' Expression ')' Statement ELSE Statement
        | SWITCH '(' Expression ')' Block
        ;

IterationStatement
	: WHILE '(' Expression ')' Statement
	| DO Statement WHILE '(' Expression ')' ';'
	| FOR '(' ForInit ForExpr ForIncr ')' Statement
	| FOR '(' ForInit ForExpr         ')' Statement
	;

ForInit
	: ExpressionStatements ';'
	| LocalVariableDeclarationStatement
	| ';'
	;

ForExpr
	: Expression ';'
	| ';'
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements ',' ExpressionStatement
	;

JumpStatement
	: BREAK IDENTIFIER ';'
	| BREAK            ';'
        | CONTINUE IDENTIFIER ';'
	| CONTINUE            ';'
	| RETURN Expression ';'
	| RETURN            ';'
	| THROW Expression ';'
	;

GuardingStatement
	: SYNCHRONIZED '(' Expression ')' Statement
	| TRY Block Finally
	| TRY Block Catches
	| TRY Block Catches Finally
	;

Catches
	: Catch
	| Catches Catch
	;

Catch
	: CatchHeader Block
	;

CatchHeader
	: CATCH '(' TypeSpecifier IDENTIFIER ')'
	| CATCH '(' TypeSpecifier ')'
	;

Finally
	: FINALLY Block
	;

PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| NewAllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: '(' Expression ')'
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL
	| BOOLLIT
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName '[' Expression ']'
	| ComplexPrimary '[' Expression ']'
	;

FieldAccess
	: NotJustName '.' IDENTIFIER
	| RealPostfixExpression '.' IDENTIFIER
        | QualifiedName '.' THIS
        | QualifiedName '.' CLASS
        | PrimitiveType '.' CLASS
	;

MethodCall
	: MethodAccess '(' ArgumentList ')'
	| MethodAccess '(' ')'
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| SpecialName
	| QualifiedName
	;

SpecialName
	: THIS
	| SUPER
	| JNULL
	;

ArgumentList
	: Expression
	| ArgumentList ',' Expression
	;

NewAllocationExpression
        : PlainNewAllocationExpression
        | QualifiedName '.' PlainNewAllocationExpression
        ;

PlainNewAllocationExpression
    	: ArrayAllocationExpression
    	| ClassAllocationExpression
    	| ArrayAllocationExpression '{' '}'
    	| ClassAllocationExpression '{' '}'
    	| ArrayAllocationExpression '{' ArrayInitializers '}'
    	| ClassAllocationExpression '{' FieldDeclarations '}'
    	;

ClassAllocationExpression
	: NEW TypeName '(' ArgumentList ')'
	| NEW TypeName '('              ')'
        ;

ArrayAllocationExpression
	: NEW TypeName DimExprs Dims
	| NEW TypeName DimExprs
        | NEW TypeName Dims
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	;

DimExpr
	: '[' Expression ']'
	;

Dims
	: OP_DIM
	| Dims OP_DIM
	;

PostfixExpression
	: PrimaryExpression
	| RealPostfixExpression
	;

RealPostfixExpression
	: PostfixExpression OP_INC
	| PostfixExpression OP_DEC
	;

UnaryExpression
	: OP_INC UnaryExpression
	| OP_DEC UnaryExpression
	| ArithmeticUnaryOperator CastExpression
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	;

LogicalUnaryOperator
	: '~'
	| '!'
	;

ArithmeticUnaryOperator
	: '+'
	| '-'
	;

CastExpression
	: UnaryExpression
	| '(' PrimitiveTypeExpression ')' CastExpression
	| '(' ClassTypeExpression ')' CastExpression
	| '(' Expression ')' LogicalUnaryExpression
	;

PrimitiveTypeExpression
	: PrimitiveType
        | PrimitiveType Dims
        ;

ClassTypeExpression
	: QualifiedName Dims
        ;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression '*' CastExpression
	| MultiplicativeExpression '/' CastExpression
	| MultiplicativeExpression '%' CastExpression
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
	| AdditiveExpression '-' MultiplicativeExpression
        ;

ShiftExpression
	: AdditiveExpression
        | ShiftExpression OP_SHL AdditiveExpression
        | ShiftExpression OP_SHR AdditiveExpression
        | ShiftExpression OP_SHRR AdditiveExpression
	;

RelationalExpression
	: ShiftExpression
        | RelationalExpression '<' ShiftExpression
	| RelationalExpression '>' ShiftExpression
	| RelationalExpression OP_LE ShiftExpression
	| RelationalExpression OP_GE ShiftExpression
	| RelationalExpression INSTANCEOF TypeSpecifier
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
        ;

AndExpression
	: EqualityExpression
        | AndExpression '&' EqualityExpression
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression '^' AndExpression
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression '|' ExclusiveOrExpression
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND InclusiveOrExpression
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR ConditionalAndExpression
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression '?' Expression ':' ConditionalExpression
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	;

AssignmentOperator
	: '='
	| ASS_MUL
	| ASS_DIV
	| ASS_MOD
	| ASS_ADD
	| ASS_SUB
	| ASS_SHL
	| ASS_SHR
	| ASS_SHRR
	| ASS_AND
	| ASS_XOR
	| ASS_OR
	;

Expression
	: AssignmentExpression
        ;

ConstantExpression
	: ConditionalExpression
	;

%%

