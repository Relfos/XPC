{
  XPC_ASTNodes.pas
  Copyright (c) 2015 by Sergio Flores <relfos@gmail.com>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
}
//callconv
Unit XPC_ASTNodes;

{$I terra.inc}

Interface
Uses TERRA_Collections;

{$M+}

Type
  ASTString = AnsiString;
  
  ScopeEnum = (scope_Public,  scope_Protected, scope_Private, scope_Published);

	MethodKindEnum = (method_Default,	method_Constructor, method_Destructor);

  ParameterKindEnum = (param_Default, param_Var, param_Out, param_Const);

  // function attributes
  FunctionAttributeEnum = (
    attribute_Assembler,
    attribute_Export,
    attribute_Inline,
    attribute_VarArgs,
    attribute_Far,
    attribute_Near,
    attribute_Resident,
    attribute_Overload,
    attribute_External,
    attribute_Forward,
    attribute_Abstract,
    attribute_Override,
    attribute_Virtual,
    attribute_Dynamic,
    attribute_Reintroduce,
    attribute_call_Pascal,
    attribute_call_SafeCall,
    attribute_call_StdCall,
    attribute_call_CDecl,
    attribute_call_Register
  );

  // operators
  ArithmeticBinaryOp = (op_ADD, op_SUB, op_DIV, op_MUL, op_QUOT, op_MOD, op_SHR, op_SHL);
	LogicalBinaryOp = (op_AND, op_OR, op_XOR);
	ComparisonBinaryOp = (op_EQ, op_NE, op_LT, op_LE, op_GT, op_GE, op_SGT, op_SGE,op_SLT,op_SLE);

  PASTNode = ^ASTNode;
  ASTNode = Class
    Public
      Function Level:Integer;

      Procedure Assign(Target:PASTNode; Child:ASTNode);

    Published
      Owner:ASTNode;
  End;

  ASTNodeType = Class Of ASTNode;

  ListNode = Class(ASTNode)
    Public
      Elements:Array Of ASTNode;
      Count:Integer;

      Constructor Create(Element:ASTNode = Nil);

      Procedure Add(Element:ASTNode); Overload;
      Procedure AddList(List:ListNode); Overload;

      Procedure InsertAt(Index:Integer; Element:ASTNode); Overload;
      Procedure InsertListAt(Index:Integer; List:ListNode); Overload;

      Function Get(Index:Integer):ASTNode;
  End;

	SourceNode = Class(ASTNode)
    Protected
      Name:ASTString;
  End;

  TypeNode = Class(ASTNode)
    Protected
  		// TODO each derive should set the typesize
	  	TypeSize:Integer;
  End;

  TypeListNode = Class(ListNode)
    Public
      Function GetType(Index:Integer):TypeNode;
  End;

  MetaTypeNode = Class(TypeNode)
    Public
      Value:ASTString;

    Constructor Create(TypeName:ASTString);
  End;

  DeclarationNode = Class(ASTNode)
    Public
  		Name:ASTString;

  		Constructor Create(Const Name:ASTString; T:TypeNode = Nil);

    Published
      DeclType:TypeNode;
  End;

  DeclarationListNode = Class(ListNode)
    Public
      Function GetDeclaration(Index:Integer):DeclarationNode;
  End;

  UnitItemNode = Class(DeclarationNode)
    Public
      Location:ASTString;

      Constructor Create(Const Name:ASTString; Const Location:ASTString = ''); 
  End;

  UnitListNode = Class(ListNode)
    Public
      Function GetUnit(Index:Integer):UnitItemNode;
  End;

	StatementNode = Class(ASTNode)
  End;

  CompositeDeclarationNode = Class;
  IdentifierListNode = Class;

	SectionNode = Class(ASTNode)
    Public
  		Constructor Create(Decls:DeclarationListNode);

    Published
  		Decls:DeclarationListNode;
  End;

  TopLevelDeclarationSectionNode = Class(SectionNode)
    Public
  		Constructor Create(UsesList:UnitListNode; Decls:DeclarationListNode);

    Published
      UsesList:UnitListNode;
  End;


	InterfaceSectionNode = Class(TopLevelDeclarationSectionNode)
  End;

	ImplementationSectionNode = Class(TopLevelDeclarationSectionNode)
  End;

  StatementListNode = Class(StatementNode)
    Public
      Constructor Create(St:StatementNode);
      Procedure Add(St:StatementNode);

      Function GetStatement(Index:Integer):StatementNode;

    Published
      List:ListNode;
  End;

	BlockStatementNode = Class(StatementNode)
    Public
  		Constructor Create(List:StatementListNode);

    Published
  		List:StatementListNode;
  End;

	ProgramSectionNode = Class(TopLevelDeclarationSectionNode)
    Public
      Constructor Create(UsesList:UnitListNode; Decls:DeclarationListNode; Code:BlockStatementNode);

    Published
      Block:BlockStatementNode;
  End;

	ProgramNode = Class(SourceNode)
    Public
      Constructor Create(Const Name:ASTString; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);

    Published
      Section:ProgramSectionNode;
  End;

	ConstantValueNode = Class(ASTNode)
  End;

  ExpressionNode = Class(ASTNode)
    Public
      EnforceConst:Boolean;

    Published
  		ForcedType:TypeNode;
  End;

  ExpressionListNode = Class(ListNode)
    Public
      Function GetExpression(Index:Integer):ExpressionNode;
  End;

  LiteralNode = Class;
  IntLiteralNode = Class;

  ConstExpressionNode = Class(ExpressionNode)
    Function ResolveToLiteral():LiteralNode;
  End;

	LabelDeclarationNode = Class(DeclarationNode)
  End;

	ValueDeclarationNode = Class(DeclarationNode)
  End;

	VarDeclarationNode = Class(ValueDeclarationNode)
    Public
		  AbsoluteID:ASTString;
	  	IsThrVar:Boolean;

		  Constructor Create(Const Name:ASTString; VarType:TypeNode; Const AbsoluteId:ASTString; Init:ExpressionNode = Nil);

    Published
  		Init:ExpressionNode;
  End;

	/// Routine parameters. May be value (default), variable, constant, or out.
	/// Param types must be an id, string or open array (array of paramtype)
	ParamDeclarationNode = Class(ValueDeclarationNode)
    Public
      Kind:ParameterKindEnum;

	  	Constructor Create(Const Name:ASTString; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum);

    Published
  		Init:ExpressionNode;
  End;

	VarParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Const Name:ASTString; VarType:TypeNode);
  End;

	ConstParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Const Name:ASTString; ConstType:TypeNode; Init:ExpressionNode = Nil);
  End;

	OutParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Const Name:ASTString; VarType:TypeNode);
  End;


  ParametersSectionNode = Class(SectionNode)
    Public

		  Constructor Create(Decls:DeclarationListNode=Nil);
		//public override bool Equals(object obj)
		{
			ParametersSection sec = obj as ParametersSection;
			return sec != null && returnVar.Equals(sec.returnVar) && decls.SequenceEqual(sec.decls);
		}

    Published
		  ReturnVar:ParamDeclarationNode; //TODO
  End;


	/// Directives constraints:
	///		Override | Abstract
	///		Abstract => virtual
	///		varargs => cdecl
  FunctionAttributeNode = Class(ASTNode)
    Public
      Value:FunctionAttributeEnum;

      Constructor Create(Value:FunctionAttributeEnum);
  End;

	FunctionDirectiveListNode  = Class(ListNode)
    Public

		  // Checks the immediate coherence between function directives.
	  	// Must be called after all directives are added
  		Function CheckDirectives():Boolean;

    Published
  		CallConv:FunctionAttributeNode;
  End;

  ProceduralTypeNode = Class;

	// Declaration of a Callable unit, i.e. a global routine or method
	CallableDeclarationNode = Class(DeclarationNode)
    Public
  		// Gets the fully qualified name of this callable
  		// (obj+metname for methods, plus parameter types for overloads)
  		// To be set by the Resolver
	  	QualifiedName:ASTString;

  		// Section that declares this callable. To be set by resolver
	  	DeclaringSection:SectionNode;

		  Constructor Create(Const Name:ASTString; Params:ParametersSectionNode; RetType:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);

    Published
      SignatureType:ProceduralTypeNode;
      ResultType:TypeNode;

      Directives:FunctionDirectiveListNode;
  End;


	RoutineSectionNode = Class(SectionNode)
    Public
  		 // to be set by resolver
	  	DeclaringCallable:CallableDeclarationNode;

  		Constructor Create(Decls:DeclarationListNode; Block:StatementNode);

    Public
      Block:StatementNode;
  End;

	LibraryNode = Class(SourceNode)
    Public
  		Constructor Create(Const Name:ASTString; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);

    Published
  		Section:ProgramSectionNode;
  End;

	UnitNode = Class(SourceNode)
    Public
  		Constructor Create(Const Name:ASTString; Interfce:InterfaceSectionNode; Impl:ImplementationSectionNode;  Init:BlockStatementNode = Nil; Final:BlockStatementNode = Nil);

    Published
		  Interfaces:InterfaceSectionNode;
	  	Implements:ImplementationSectionNode;
  		Inits:BlockStatementNode;
  		Final:BlockStatementNode;
  End;

  PackageNode = Class(SourceNode)
    Public
  		Constructor Create(Const Name:ASTString; requires, contains:UnitListNode);

    Published
  		Requires:UnitListNode;
	  	Contains:UnitListNode;
  End;

  StructuredConstantNode = Class(ConstExpressionNode)
    Public
  		Constructor Create(ExprList:ExpressionListNode);

    Published
      ExprList:ExpressionListNode;
  End;

	ArrayConstNode = Class(StructuredConstantNode)
    Public
      Constructor Create(ExprList:ExpressionListNode); Overload;
	  	Constructor Create(Const ArrayElems:ASTString); Overload;
			//: base(new ExpressionList(arrayElems.ToCharArray().Select(x => new CharLiteral(x)))){ }
	End;

  FieldInitNode = Class(ConstExpressionNode)
    Public
  	  FieldName:ASTString;

      Constructor Create(Const Name:ASTString; Expr:ExpressionNode);

    Published
      Expr:ExpressionNode;
  End;

  FieldInitListNode = Class(ExpressionListNode)
    Constructor Create(F:FieldInitNode = Nil);
    //Procedure Add(F:FieldInit);
  End;

  RecordConstNode = Class(StructuredConstantNode)
    Constructor Create(ExprList:FieldInitListNode);
  End;

  ConstIdentifierNode = Class(ConstExpressionNode)
    Public
      Name:ASTString;

      Constructor Create(Const Name:ASTString);
  End;

  ScalarTypeNode = Class(TypeNode)
  End;

  TypeClassNode = Class Of ScalarTypeNode;

  IntegralTypeNode = Class(ScalarTypeNode)
    Function GetMinValue():Int64 ; Virtual; Abstract;
    Function GetMaxValue():Int64 ; Virtual; Abstract;
  End;

	IntegerTypeNode = Class(IntegralTypeNode)
  End;

	SignedIntegerTypeNode = Class(IntegerTypeNode)
  End;

	UnsignedIntegerTypeNode = Class(IntegerTypeNode)
  End;

(*	UnsignedInt8Type = Class(UnsignedIntegerType)		// byte
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	UnsignedInt16Type = Class(UnsignedIntegerType)	// word
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	UnsignedInt32Type = Class(UnsignedIntegerType)	// cardinal
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	UnsignedInt64Type = Class(UnsignedIntegerType)	// uint64
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	SignedInt8Type = Class(SignedIntegerType)			// smallint
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	SignedInt16Type = Class(SignedIntegerType)		// smallint
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	SignedInt32Type = Class(SignedIntegerType)		// integer
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	SignedInt64Type = Class(IntegerType)				// int64
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;*)

	BoolTypeNode = Class(IntegralTypeNode)
	End;

	CharTypeNode = Class(IntegralTypeNode)
	End;

	RealTypeNode = Class(ScalarTypeNode)
  End;

	FloatTypeNode = Class(RealTypeNode)
  End;

	DoubleTypeNode = Class(RealTypeNode)
  End;

	ExtendedTypeNode = Class(RealTypeNode)
  End;

	CurrencyTypeNode = Class(RealTypeNode)
  End;

	StringTypeNode = Class(ScalarTypeNode)
    Public
      Constructor Create(Len:ExpressionNode = Nil);

    Published
      Length:ExpressionNode;
	End;

	FixedStringTypeNode = Class(StringTypeNode)
    Public
  		Len:Integer;

	  	Constructor Create(Expr:ExpressionNode);

    Published
  		Expr:ExpressionNode;
  End;

	RangeTypeNode = Class(TypeNode)
    Public
  		Constructor Create(Min, Max:ExpressionNode);

    Published
  		Min:ExpressionNode;
	  	Max:ExpressionNode;
  End;

  EnumValueNode = Class;

  EnumValueListNode = Class(ListNode)
    Public
      Function GetEnum(Index:Integer):EnumValueNode;
  End;

	EnumTypeNode = Class(TypeNode)
    Public
  		Constructor Create(EnumVals:EnumValueListNode);

    Published
      List:EnumValueListNode;
  End;

	/// Variants can hold values of any type except records, sets, static arrays, files, classes, class references, and pointers.
	/// I.e. can hold anything but structured types and pointers.
	/// They can hold interfaces, dynamic arrays, variant arrays
	VariantTypeNode = Class(TypeNode)
    Published
  		ActualType:TypeNode;
  End;

	/// PointedType may be any type.
	/// It may be a not yet declared type (forward declaration)
	PointerTypeNode = Class(ScalarTypeNode)
    Public
  		Constructor Create(PointedType:TypeNode);

    Published
  		PointedType:TypeNode;
  End;


	PropertySpecifiersNode = Class(ASTNode)
		Public
		  Read:ASTString;
		  Write:ASTString;
		  Impl:ASTString;

		  Constructor Create(Const Read, Write:ASTString); Overload;
		  Constructor Create(Index:IntLiteralNode; Const Read, Write:ASTString; Stored:ConstExpressionNode; Default:LiteralNode; Const Impl:ASTString =''); Overload;

    Published
      Index:IntLiteralNode;
      Stored:ConstExpressionNode;
		  Default:LiteralNode;	// nodefault == Int32.MaxValue
  End;


  ObjectSectionNode = Class;

  CompositeTypeNode = Class(TypeNode)
    Public
  		IsPacked:Boolean;
  		// optional
	  	Name:ASTString;

      //TODO
      Ancestors:Array Of CompositeTypeNode;
      AncestorCount:Integer;

  		//public List<CompositeType> ancestors;

	  	Function IsForward:Boolean;

    Published
  		Heritage:IdentifierListNode;
	  	Section:ObjectSectionNode;
  		Decl:CompositeDeclarationNode;
  End;

	FieldDeclarationNode = Class(ValueDeclarationNode)
    Public
  		isStatic:Boolean;
	  	Scope:ScopeEnum;

  		Constructor FieldDeclaration(Const Name:ASTString; T:TypeNode = Nil; IsStatic:Boolean = False);

    Published
  		DeclaringObject:CompositeTypeNode;
  End;

	PropertyDeclarationNode = Class(FieldDeclarationNode)
    Public
		  IsStatic:Boolean;

		  Constructor Create(Const Name:ASTString; T:TypeNode; Specs:PropertySpecifiersNode = Nil);

    Published
  		Specifiers:PropertySpecifiersNode;
  End;

	ProceduralTypeNode = Class(TypeNode)
    Public
  		Constructor Create(Params:ParametersSectionNode; ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);

    Published
  		Params:ParametersSectionNode;

  		/// Function's return type. Must be null for every non-function routine.
	  	FuncRet:TypeNode;

		  Directives:FunctionDirectiveListNode;
  End;

	MethodTypeNode = Class(ProceduralTypeNode)
    Public
  		Kind:MethodKindEnum;
  End;

	/// Declaration of a Method
	MethodDeclarationNode = Class(CallableDeclarationNode)
    Public
  		isStatic:Boolean;
  		Objname:ASTString;
  		Name:ASTString;
      Scope:ScopeEnum;
      Kind:MethodKindEnum;

	  	Constructor Create(Const Objname, Name:ASTString; params:ParametersSectionNode; ret:TypeNode = Nil; dirs:FunctionDirectiveListNode  = Nil; kind:MethodKindEnum  = method_Default);

    Published
	  	DeclaringObject:CompositeTypeNode;
	End;

	ObjectSectionNode = Class(SectionNode)
    Public
  		Constructor Create(Fields:DeclarationListNode = Nil; Decls:DeclarationListNode = Nil; Scope:ScopeEnum = Scope_Published);

  		Procedure Add(Sec:ObjectSectionNode);

	  	Procedure AddFields(Fields:DeclarationListNode; Scope:ScopeEnum);

  		Procedure AddMethods(Methods:DeclarationListNode; Scope:ScopeEnum);

	  	Procedure AddProperties(Properties:DeclarationListNode; Scope:ScopeEnum);

  		/// Add unknown-type declarations
	  	Procedure AddDecls(Decls:DeclarationListNode; Scope:ScopeEnum);

		/// Fields, Methods and Properties
		{
		public IEnumerable<Declaration> Decls(Scope s = (Scope) 0xffffff)
			foreach (var f in fields.Cast<FieldDeclaration>().Where(f => (f.scope & s) != 0))
				yield return f;
			foreach (var d in decls.Cast<MethodDeclaration>().Where(d => (d.scope & s) != 0))
				yield return d;
			foreach (var p in properties.Cast<PropertyDeclaration>().Where(p => (p.scope & s) != 0))
				yield return p;
		}


		  /// Returns a member with the given name
  		Function GetMember(id:AnsiString):DeclarationNode;

	  	/// Returns a method with the given name
		  Function GetMethod(id:AnsiString):MethodDeclarationNode;

  		/// Returns a field with the given name
	  	Function GetField(id:AnsiString):FieldDeclarationNode;

  		/// Returns a property with the given name
	  	Function GetProperty(id:AnsiString):PropertyDeclarationNode;

    Published
		  Fields:DeclarationListNode;
		  Properties:DeclarationListNode;
      //Methods:DeclarationList;
  		DeclaringObject:CompositeTypeNode;
  End;


	ArrayPropertyNode = Class(PropertyDeclarationNode)
    Public
  		IsDefault:Boolean;

	  	Constructor Create(Const Name:ASTString; T:TypeNode; Indexes:DeclarationListNode; Specs:PropertySpecifiersNode; Def:Boolean);

    Published
  		Indexes:DeclarationListNode;
	End;

  ClassTypeNode = Class(CompositeTypeNode)
    Public
		  Constructor Create(Heritage:IdentifierListNode; Sec:ObjectSectionNode  = Nil);

    Published
  		_Self:FieldDeclarationNode;
  End;

	ClassRefTypeNode = Class(ClassTypeNode)
    Public
  		QualifID:ASTString;

	  	Constructor Create(reftype:ClassTypeNode); Overload;
		  Constructor Create(Const Name:ASTString; reftype:ClassTypeNode = Nil); Overload;

    Published
  		RefType:ClassTypeNode;
  End;

	MetaClassTypeNode = Class(ScalarTypeNode)
    Public
  		Constructor Create(baseType:TypeNode);

    Published
  		BaseType:TypeNode;
  End;

  LiteralNode = Class(ConstExpressionNode)
    //Constructor Create(Val:ConstantValue; T:TypeNode);
  End;

	OrdinalLiteralNode = Class(LiteralNode)
    //Constructor Create(V:Cardinal; T:IntegralType);
  End;

	IntLiteralNode = Class(OrdinalLiteralNode)
    Public
      Value:Int64;

      Constructor Create(Value:Int64);
  End;

	CharLiteralNode = Class(OrdinalLiteralNode)
    Public
      Value:AnsiChar;

      Constructor Create(Value:AnsiChar);
  End;

	BoolLiteralNode = Class(OrdinalLiteralNode)
    Public
      Value:Boolean;

      Constructor Create(Value:Boolean);
  End;

	StringLiteralNode = Class(LiteralNode)
    Public
      Value:ASTString;

      Constructor Create(Const Value:ASTString);
  End;

	RealLiteralNode = Class(LiteralNode)
    Public
      Value:Double;

      Constructor Create(Value:Double);
  End;

	NullLiteralNode = Class(LiteralNode)
    Public
      Constructor Create();
  End;

	BinaryExpressionNode = Class(ExpressionNode)
    Public
      Constructor Create(A, B:ExpressionNode);

    Published
	  	Left:ExpressionNode;
  		Right:ExpressionNode;
  End;

  // eg: if (A in B) then
	InExpressionNode = Class(BinaryExpressionNode)
    Public
  		Constructor Create(A, B:ExpressionNode);

    Published
  		Expr:ExpressionNode;
	  	_Set:ExpressionNode;		// enforce that 'set' is in fact a set
  End;

	SetRangeNode = Class(BinaryExpressionNode)
    Public

  		Constructor Create(_type:RangeTypeNode);
  		{
	  		this.ForcedType = this.Type = type;
		  	this.EnforceConst = true;
  		}

    Published
      Range:RangeTypeNode;
  End;

	ArithmeticBinaryExpressionNode  = Class(BinaryExpressionNode)
    Public
      Op:ArithmeticBinaryOp;
  End;

	SubtractionNode = Class(ArithmeticBinaryExpressionNode)
  End;

	AdditionNode = Class(ArithmeticBinaryExpressionNode)
	End;

	ProductNode = Class(ArithmeticBinaryExpressionNode)
  End;

	DivisionNode = Class(ArithmeticBinaryExpressionNode)
  End;

	// Integer division
	QuotientNode = Class(ArithmeticBinaryExpressionNode)
  End;

	ModulusNode = Class(ArithmeticBinaryExpressionNode)
  End;

	ShiftRightNode = Class(ArithmeticBinaryExpressionNode)
  End;

	ShiftLeftNode = Class(ArithmeticBinaryExpressionNode)
  End;

	LogicalBinaryExpressionNode = Class(BinaryExpressionNode)
    Public
		  Op:LogicalBinaryOp;
  End;

	ComparisonBinaryExpressionNode = Class(BinaryExpressionNode)
    Public
  		Op:ComparisonBinaryOp;
  End;

  LogicalAndNode = Class(LogicalBinaryExpressionNode)
  End;

	LogicalOrNode = Class(LogicalBinaryExpressionNode)
  End;


	LogicalXorNode = Class(LogicalBinaryExpressionNode)
  End;


	EqualNode = Class(ComparisonBinaryExpressionNode)
  End;


	NotEqualNode = Class(ComparisonBinaryExpressionNode)
  End;


	LessThanNode = Class(ComparisonBinaryExpressionNode)
  End;


	LessOrEqualNode = Class(ComparisonBinaryExpressionNode)
  End;


	GreaterThanNode = Class(ComparisonBinaryExpressionNode)
  End;


	GreaterOrEqualNode = Class(ComparisonBinaryExpressionNode)
  End;

	TypeBinaryExpressionNode = Class(BinaryExpressionNode)
    Public
  		Constructor Create(Expr:ExpressionNode; ExprType:TypeNode);

    Published
  		Expr:ExpressionNode;
  		Types:TypeNode;
  End;

  // eg: if A is B Then
	IsExpressionNode = Class(TypeBinaryExpressionNode)
  End;

	RuntimeCastNode = Class(TypeBinaryExpressionNode)
  End;

	UnaryExpressionNode = Class(ExpressionNode)
  End;

	SimpleUnaryExpressionNode = Class(ExpressionNode)
    Public
  		Constructor Create(Expr:ExpressionNode);

    Published
  		Expr:ExpressionNode;
  End;

	UnaryPlusNode = Class(SimpleUnaryExpressionNode)
  End;

	UnaryMinusNode = Class(SimpleUnaryExpressionNode)
  End;

	LogicalNotNode = Class(SimpleUnaryExpressionNode)
  End;

	AddressLvalueNode = Class(SimpleUnaryExpressionNode)
  End;

	SetExpressionNode = Class(UnaryExpressionNode)
    Public
  		Constructor Create(Elements:ExpressionListNode = Nil);

    Published
  		Elements:ExpressionListNode;
  End;

//Pretty simply, an rvalue is when the expression result will not survive past the end of said expression. An lvalue will.
// This basic principle is what enables move semantics and rvalue references- that you can modify them without issue, because you know that object's life is over.
  LvalueExpressionNode = Class;

	/// Cast an lvalue to an rvalue (Expr)
	LValueAsExprNode = Class(UnaryExpressionNode)
    Public
  		Constructor Create(lval:LvalueExpressionNode);

    Published
  		lval:LvalueExpressionNode;
  End;

	LvalueExpressionNode = Class(UnaryExpressionNode)
  End;

	/// Cast an rvalue (Expr) to an lvalue
	ExprAsLvalueNode = Class(LvalueExpressionNode)
    Public
  		Constructor Create(Expr:ExpressionNode);

    Published
  		Expr:ExpressionNode;
  End;

	/// eg: VarType(expr)
	StaticCastNode = Class(LvalueExpressionNode)
    Public
      CastPrimitive:TypeClassNode;

  		Constructor Create(T:TypeNode; Expr:ExpressionNode); Overload;
      Constructor Create(T:TypeClassNode; Expr:ExpressionNode); Overload;

    Published
  		CastType:TypeNode;
	  	Expr:ExpressionNode;
  End;

	ArrayAccessNode = Class(LvalueExpressionNode)
    Public
  		Constructor Create(_array:ArrayConstNode; Acessors:ExpressionListNode); Overload;
	  	Constructor Create(lvalue:LvalueExpressionNode; Acessors:ExpressionListNode); Overload;

    Published
  		LValue:LvalueExpressionNode;
  		Acessors:ExpressionListNode;
  		// object alternative to lvalue
  		_Array:ArrayConstNode;
  End;

	PointerDereferenceNode = Class(LvalueExpressionNode)
    Public
  		Constructor Create(Expr:ExpressionNode);

    Published
  		Expr:ExpressionNode;
  End;

	RoutineCallNode = Class(LvalueExpressionNode)
    Public
  		Constructor Create(Func:LvalueExpressionNode; RetType:TypeNode = Nil);
	  //RoutineCall(LvalueExpression func, ExpressionList args, TypeNode retType = null)

    Published
  		Func:LvalueExpressionNode;
	  	Args:ExpressionListNode;
  End;

	InheritedCallNode = Class(RoutineCallNode)
    Public
  		FuncName:ASTString;
	  	// to be set by resolver
  		DeclaringObject:CompositeTypeNode;

	  	Constructor Create(Const FuncName:ASTString = ''; Args:ExpressionListNode =Nil);
  End;

	/// An access to a member in an object (record, class or interface)
	ObjectAccessNode = Class(LvalueExpressionNode)
    Public
  		Field:ASTString;

	  	Constructor Create(Obj:LvalueExpressionNode; Const Field:ASTString);

    Published
  		Obj:LvalueExpressionNode;
  End;

	/// Identifier that refers to a named declaration
	IdentifierNode = Class(LvalueExpressionNode)
    Public
      Name:ASTString;

	  	Constructor Create(Const Name:ASTString; T:TypeNode = Nil);

    Published
  		Decl:DeclarationNode;
  End;

  IdentifierListNode = Class(ListNode)
    Public
      Function GetIdentifier(Index:Integer):IdentifierNode;
  End;

	/// Identifier that refers to a named class. Static access
	IdentifierStaticNode = Class(IdentifierNode)
  End;

	UnresolvedLvalueNode = Class(LValueExpressionNode)
	End;

	UnresolvedIdNode = Class(UnresolvedLvalueNode)
    Public
  		Constructor Create(ID:IdentifierNode);

    Published
  		ID:IdentifierNode;
  End;

	/// Call, to be resolver after parsing
	UnresolvedCallNode = Class(UnresolvedLvalueNode)
    Public
  		Constructor Create(lval:LvalueExpressionNode; Args:ExpressionListNode = Nil);

    Published
  		Func:LvalueExpressionNode;
	  	Args:ExpressionListNode;
  End;


	/// TODO!! Must Derive type
	ConstDeclarationNode = Class(ValueDeclarationNode)
		Public
  		Constructor Create(Const Name:ASTString; Init:ExpressionNode; T:TypeNode = Nil);

    Published
      Init:ExpressionNode;
  End;

	EnumValueNode = Class(ConstDeclarationNode)
    Constructor Create(Const Name:ASTString; Init:ExpressionNode = Nil);
  End;

	/// Creates a custom, user-defined name for some Type
	TypeDeclarationNode  = Class(DeclarationNode)
  End;

	/// Declaration of a global Routine
	RoutineDeclarationNode = Class(CallableDeclarationNode)
		Constructor Create(Const Name:ASTString; Params:ParametersSectionNode; Ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);
  End;

	/// Routine definition (implementation)
	RoutineDefinitionNode = Class(RoutineDeclarationNode)
    Public
  		Constructor Create(Const name:ASTString; params:ParametersSectionNode; ret:TypeNode = Nil; dirs:FunctionDirectiveListNode = Nil; body:RoutineSectionNode = Nil); Overload;
      Constructor Create(Const name:ASTString; signatureType:ProceduralTypeNode; dirs:FunctionDirectiveListNode = Nil; body:RoutineSectionNode = Nil); Overload;

    Published
  		Body:RoutineSectionNode;
  End;

	/// Method definition (implementation)
	MethodDefinitionNode = Class(MethodDeclarationNode)
		Public
  		Constructor Create(Const objname, Name:ASTString; Params:ParametersSectionNode; Ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil; Kind:MethodKindEnum  = Method_Default; Body:RoutineSectionNode = Nil);

    Published
      Body:RoutineSectionNode;
  End;

	CompositeDeclarationNode = Class(TypeDeclarationNode)

		Constructor Create(Const Name:ASTString; ctype:CompositeTypeNode);
  End;

	ClassDeclarationNode = Class(CompositeDeclarationNode)
  End;

  InterfaceTypeNode = Class;

	InterfaceDeclarationNode = Class(CompositeDeclarationNode)
  End;

	/// Routine Directives

 {	MethodDirectives = Class(FunctionDirectiveList)
		methoddirs:Array Of FunctionDirective;

		Procedure Add(dir:FunctionDirective);
		Function Contains(dir:FunctionDirective):Boolean;
		Function CheckDirectives():Boolean;
  End;}

	ExternalDirectiveNode = Class(ASTNode)
    Public
  		Constructor Create(_file:ExpressionNode; Name:ExpressionNode = Nil);

    Published
  		_File:ExpressionNode;
	  	Name:ExpressionNode;
  End;

	ImportDirectivesNode = Class(FunctionDirectiveListNode)
    Public
      Constructor Create(ImportDir:FunctionAttributeNode);

    Published
  		ImportDir:FunctionAttributeNode;
	  	_external:ExternalDirectiveNode;
  End;

	LabelStatementNode = Class(StatementNode)
    Public
  		Name:ASTString;

	  	// to be set by the resolver
		  Decl:LabelDeclarationNode;

  		Constructor Create(Const Name:ASTString; Statement:StatementNode);

    Published
      Statement:StatementNode;
  End;

	GotoStatementNode = Class(StatementNode)
    Public
  		GotoLabel:ASTString;

	  	// to be set by the resolver
		  Decl:LabelDeclarationNode;

  		Constructor Create(Const LabelName:ASTString);
  End;

	EmptyStatementNode = Class(StatementNode)
  End;

	BreakStatementNode = Class(StatementNode)
  End;

	ContinueStatementNode = Class(StatementNode)
  End;

	AssignmentNode = Class(StatementNode)
    Public
  		Constructor Create(lvalue:LvalueExpressionNode; Expr:ExpressionNode);

    Published
  		lvalue:LvalueExpressionNode;
	  	Expr:ExpressionNode;
  End;

	IfStatementNode = Class(StatementNode)
    Public
  		Constructor Create(condition:ExpressionNode ; ifTrue:StatementNode; ifFalse:StatementNode=Nil);

    Published
		  Condition:ExpressionNode;
	  	ThenBlock:StatementNode;
  		ElseBlock:StatementNode;
  End;

	ExpressionStatementNode = Class(StatementNode)
    Public
  		Constructor Create(Expr:ExpressionNode);

    Published
  		Expr:ExpressionNode;
  End;

	CaseSelectorNode = Class(StatementNode)
    Public
  		Constructor Create(List:ExpressionListNode; Body:StatementNode);

    Published
	  	List:ExpressionListNode;
  		Body:StatementNode;
  End;

	CaseStatementNode = Class(StatementNode)
    Public
  		Constructor Create(Condition:ExpressionNode; Selectors:StatementListNode; CaseElse:StatementNode);

    Published
  		Condition:ExpressionNode;
  		Selectors:StatementListNode;
	  	CaseElse:StatementNode;
  End;

	LoopStatementNode = Class(StatementNode)
    Public
  		Constructor Create(Body:StatementNode; Condition:ExpressionNode);

    Published
  		Condition:ExpressionNode;
  		Body:StatementNode;
  End;

	RepeatLoopNode = Class(LoopStatementNode)
  End;

	WhileLoopNode = Class(LoopStatementNode)
  End;

	ForLoopNode = Class(LoopStatementNode)
    Public
  		Direction:Integer;

	  	Constructor Create(_var:IdentifierNode; start:ExpressionNode; _End:ExpressionNode; Body:StatementNode; Dir:Integer);

    Published
		  _var:IdentifierNode;
	  	Start:ExpressionNode;
  		_End:ExpressionNode;
  End;

	WithStatementNode = Class(StatementNode)
    Public
  		Constructor Create(_with:ExpressionListNode; Body:StatementNode);

    Published
	  	_With:ExpressionListNode;
  		Body:StatementNode;
  End;

	TryFinallyStatementNode = Class(StatementNode)
    Public
  		Constructor Create(Body, Final:BlockStatementNode);

    Published
  		Body:BlockStatementNode;
	  	Final:BlockStatementNode;
  End;

	ExceptionBlockNode = Class(StatementNode)
    Public
  		Constructor Create(onList:StatementListNode; Default:BlockStatementNode = Nil);

    Published
    	onList:StatementListNode;
	  	Default:BlockStatementNode;	// else or default, same semantics
  End;

	TryExceptStatementNode = Class(StatementNode)
    Public
  		Constructor Create(Body:BlockStatementNode; Final:ExceptionBlockNode);

    Published
  		Body:BlockStatementNode;
	  	Final:ExceptionBlockNode;
  End;


	RaiseStatementNode = Class(StatementNode)
    Public
  		Constructor Create(lvalue:LvalueExpressionNode; Expr:ExpressionNode);

    Published
  		LValue:LvalueExpressionNode;
	  	Expr:ExpressionNode;
  End;

	OnStatementNode = Class(StatementNode)
    Public
  		Ident:ASTString;
  		_type:ASTString;

  		Constructor Create(Const Ident, _type:ASTString; Body:StatementNode);

    Published
  		Body:StatementNode;
  End;

	AssemblerBlockNode = Class(BlockStatementNode)
    Public
  		Constructor AssemblerBlock(Body:StatementListNode);
  End;

	InterfaceTypeNode = Class(CompositeTypeNode)
    Public
  		Constructor Create(Heritage:IdentifierListNode; Ssec:ObjectSectionNode = Nil; guid:StringLiteralNode  = Nil);

    Published
      Ssec:ObjectSectionNode;
	  	Guid:StringLiteralNode;
  End;


	///			StructuredType > Type
	///				Array > Type
	///				Set	  > OrdinalType
	///				File  > VariableType
	///		 		Record > TypeNode ...

	StructuredTypeNode = Class(TypeNode)
    Public
  		IsPacked:Boolean;
		{
		public override bool Equals(Object o)
			if (!(o is StructuredType))
				return false;

			StructuredType otype = (StructuredType) o;
			if (IsPacked != otype.IsPacked)
				return false;

			return basetype.Equals(otype.basetype);
		}
    Published
  		BaseType:TypeNode;
  End;

	RecordTypeNode = Class(StructuredTypeNode)
    Public
  		Constructor Create(compTypes:DeclarationListNode);

    Published
  		CompTypes:DeclarationListNode;
  End;

	RecordFieldDeclarationNode = Class(ValueDeclarationNode)
  End;

  ArrayTypeNode = Class(StructuredTypeNode)
    Public
    	Dimensions:Array Of Integer;
      DimensionCount:Integer;

    	Constructor Create(baseType:TypeNode; Dims:TypeListNode); Overload;
  	  Constructor Create(baseType, SizeType:TypeNode); Overload;
      Constructor Create(sizeType:TypeNode); Overload;

    	Procedure AddDimension(size:Integer);
  End;

  SetTypeNode = Class(StructuredTypeNode)
    Public
      Constructor Create(T:TypeNode);
  End;

  FileTypeNode = Class(StructuredTypeNode)
    Public
      Constructor Create(T:TypeNode);
  End;

	VariantDeclarationNode = Class(RecordFieldDeclarationNode)
    Public
  		Constructor Create(Const Name:ASTString; T:TypeNode; Fields:DeclarationListNode);

    Published
  		Fields:DeclarationListNode;
  End;

	/// Variant case entry declaration
	VarEntryDeclarationNode = Class(RecordFieldDeclarationNode)
    Public
  		Constructor Create(TagValue:ExpressionNode; Fields:DeclarationListNode);	// type must be later set to the variant type

    Published
  		TagValue:ExpressionNode;
  		Fields:RecordTypeNode;
	End;

  ExportItemNode = Class(UnitItemNode)
    Public
  		ExportName:ASTString;
  		Index:Integer;

	  	Constructor Create(Const Name:ASTString; pars:ParametersSectionNode; ExportName:ASTString = ''); Overload;
      Constructor Create(Const Name:ASTString; pars:ParametersSectionNode; Index:Integer); Overload;

    Published
  		FormalParams:ParametersSectionNode;
  End;

	UnresolvedNode = Class(TypeNode)
    Public
  		ID:ASTString;

	  	Constructor Create(Const Name:ASTString);
  End;

	UnresolvedTypeNode = Class(UnresolvedNode)
  End;

	UnresolvedVariableTypeNode = Class(UnresolvedNode)
  End;

	UnresolvedIntegralTypeNode = Class(UnresolvedNode)
  End;


	UnresolvedOrdinalTypeNode = Class(UnresolvedNode)
  End;


Implementation
Uses TERRA_Error;

{ ASTNode }
Function ASTNode.Level: Integer;
Begin
  If Assigned(Owner) Then
    Result := Succ(Owner.Level)
  Else
    Result := 0;
End;


Procedure ASTNode.Assign(Target:PASTNode; Child:ASTNode);
Begin
  If Assigned(Child) Then
    Child.Owner := Self;
    
  Target^ := Child;
End;


{ MetaType }
Constructor MetaTypeNode.Create(TypeName:ASTString);
Begin
  Self.Value := TypeName;
End;

{ Declaration }
Constructor DeclarationNode.Create(Const Name: ASTString; T: TypeNode);
Begin
  Self.Name := Name;
  Assign(@DeclType, T);
End;

{ ListNode }
Constructor ListNode.Create(Element:ASTNode);
Begin
  Self.Count := 0;
  Self.Add(Element);
End;

Procedure ListNode.Add(Element:ASTNode);
Begin
  If Element = Nil Then
    Exit;

  If (Element Is ListNode) Then
  Begin
    Self.AddList(ListNode(Element));
    Exit;
  End;

  Inc(Count);
  SetLength(Elements, Count);
  Assign(@Elements[Pred(Count)], Element);
End;

Procedure ListNode.AddList(List: ListNode);
Var
  I:Integer;
Begin
  If List = Nil Then
    Exit;

  For I:=0 To Pred(List.Count) Do
    Self.Add(List.Elements[I]);
End;

Procedure ListNode.InsertListAt(Index:Integer; List:ListNode);
Var
  I, Prev:Integer;
Begin
  If List = Nil Then
    Exit;

  If (Index>=Count) Then
  Begin
    Self.Add(List);
    Exit;
  End;

  Prev := Count;
  Inc(Count, List.Count);
  SetLength(Elements, Count);

  For I:=Index To Pred(Prev) Do
    Elements[I + List.Count] := Elements[I];

  For I:=0 To Pred(List.Count) Do
    Assign(@Elements[I+Prev], List.Elements[I]);
End;

Procedure ListNode.InsertAt(Index:Integer; Element:ASTNode);
Var
  I:Integer;
Begin
  If List = Nil Then
    Exit;

  If (Element Is ListNode) Then
  Begin
    Self.InsertListAt(Index, ListNode(Element));
    Exit;
  End;

  If (Index>=Count) Then
  Begin
    Self.Add(Element);
    Exit;
  End;

  Inc(Count);
  SetLength(Elements, Count);

  For I:=Index To (Count - 2) Do
    Elements[I + 1] := Elements[I];

  Assign(@Elements[I+Pred(Count)], Element);
End;

Function ListNode.Get(Index: Integer): ASTNode;
Begin
  If (Index<0) Or (Index>=Count) Then
    Result := Nil
  Else
    Result := Elements[Index];
End;

{ UnitItemNode }
Constructor UnitItemNode.Create(Const Name, Location: ASTString);
Begin
  Self.Name := Name;
  Self.Location := Location;
  Self.DeclType := Nil;
End;

{ ProgramNode }
Constructor ProgramNode.Create(Const Name:ASTString; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
Begin
  Self.Name := Name;
  Assign(@Self.Section, ProgramSectionNode.Create(UsesList, Decls, Body));
End;

{ CompositeTypeNode }
Function CompositeTypeNode.IsForward: Boolean;
Begin
  Result := (Section = Nil);
End;

{ SectionNode }
Constructor SectionNode.Create(Decls: DeclarationListNode);
Begin
  Assign(@Self.Decls, Decls);
End;

{ TopLevelDeclarationSectionNode }
Constructor TopLevelDeclarationSectionNode.Create(UsesList:UnitListNode; Decls:DeclarationListNode);
Begin
  Assign(@Self.Useslist, UsesList);
  Assign(@Self.Decls, Decls);
End;

{ BlockStatementNode }
Constructor BlockStatementNode.Create(List: StatementListNode);
Begin
  Assign(@Self.List, List);
End;

{ ProgramSectionNode }
Constructor ProgramSectionNode.Create(UsesList: UnitListNode; Decls: DeclarationListNode; Code: BlockStatementNode);
Begin
  Assign(@Self.Block, Code);
  Assign(@Self.Useslist, UsesList);
  Assign(@Self.Decls, Decls);
End;

{ ConstExpressionNode }
Function ConstExpressionNode.ResolveToLiteral: LiteralNode;
Begin
  Result := Nil; //TODO
End;

{ VarDeclarationNode }
Constructor VarDeclarationNode.Create(Const Name:ASTString; VarType:TypeNode; Const AbsoluteId:ASTString; Init:ExpressionNode);
Begin
  Self.Name := Name;
  Self.AbsoluteID := AbsoluteID;
  Assign(@Self.DeclType, VarType);
  Assign(@Self.Init, Init);
End;


{ ParamDeclarationNode }
Constructor ParamDeclarationNode.Create(Const Name:ASTString; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, ParamType);
  Assign(@Self.Init, Init);
  Self.Kind := Kind;
End;

{ VarParamDeclarationNode }
Constructor VarParamDeclarationNode.Create(Const Name:ASTString; VarType:TypeNode);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, VarType);
End;

{ ConstParamDeclarationNode }
Constructor ConstParamDeclarationNode.Create(Const Name:ASTString; ConstType:TypeNode; Init:ExpressionNode);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, ConstType);
  Assign(@Self.Init, Init);
End;

{ OutParamDeclarationNode }
Constructor OutParamDeclarationNode.Create(Const Name:ASTString; VarType:TypeNode);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, VarType);
End;

{ ParametersSectionNode }
Constructor ParametersSectionNode.Create(Decls: DeclarationListNode);
Begin
  Assign(@Self.Decls, Decls);
End;

{ FunctionDirectiveListNode }
Function FunctionDirectiveListNode.CheckDirectives: Boolean;
Begin
  // TODO
End;

{ CallableDeclarationNode }

Constructor CallableDeclarationNode.Create(Const Name:ASTString; Params:ParametersSectionNode; RetType:TypeNode; Dirs:FunctionDirectiveListNode);
Begin
  Self.Name := Name;
  Assign(@Self.DeclaringSection, Params);
  Assign(@Self.ResultType, RetType);
  Assign(@Self.Directives, Dirs);
End;

{ RoutineSectionNode }

Constructor RoutineSectionNode.Create(Decls: DeclarationListNode; Block: StatementNode);
Begin
  Assign(@Self.Decls, Decls);
  Assign(@Self.Block, Block);
End;

{ LibraryNode }
Constructor LibraryNode.Create(Const Name:ASTString; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
Begin
  Self.Name := Name;
  Assign(@Self.Section, ProgramSectionNode.Create(UsesList, Decls, Body));
End;

{ UnitNode }
Constructor UnitNode.Create(Const Name:ASTString; Interfce:InterfaceSectionNode; Impl:ImplementationSectionNode; Init, Final: BlockStatementNode);
Begin
  Self.Name := Name;
  Assign(@Self.Interfaces, Interfce);
  Assign(@Self.Implements, Impl);
  Assign(@Self.Inits, Init);
  Assign(@Self.Final, Final);
End;

{ PackageNode }
Constructor PackageNode.Create(Const Name:ASTString; Requires, Contains: UnitListNode);
Begin
  Self.Name := Name;
  Assign(@Self.Requires, Requires);
  Assign(@Self.Contains, Contains);
End;

{ StructuredConstantNode }

Constructor StructuredConstantNode.Create(ExprList: ExpressionListNode);
Begin
  Assign(@Self.ExprList, ExprList);
End;

{ ArrayConstNode }
Constructor ArrayConstNode.Create(ExprList: ExpressionListNode);
Begin
  Assign(@Self.ExprList, ExprList);
End;

Constructor ArrayConstNode.Create(Const ArrayElems: ASTString);
Begin
  RaiseError('TODO');
End;

{ FieldInitNode }
Constructor FieldInitNode.Create(Const Name:ASTString; Expr: ExpressionNode);
Begin
  Self.FieldName := Name;
  Assign(@Self.Expr, Expr);
End;

{ FieldInitListNode }
Constructor FieldInitListNode.Create(F: FieldInitNode);
Begin
  RaiseError('TODO');
End;

{ RecordConstNode }
Constructor RecordConstNode.Create(ExprList: FieldInitListNode);
Begin
  Assign(@Self.ExprList, ExprList);
End;

{ ConstIdentifierNode }

Constructor ConstIdentifierNode.Create(Const Name:ASTString);
Begin
  Self.Name := Name;
End;

(*
{ UnsignedInt8TypeNode }
function UnsignedInt8Type.GetMaxValue: Int64;
Begin
  Result := 255;
End;

function UnsignedInt8Type.GetMinValue: Int64;
Begin
  Result := 0;
End;

{ UnsignedInt16TypeNode }

function UnsignedInt16Type.GetMaxValue: Int64;
Begin
  Result := 65535;
End;

function UnsignedInt16Type.GetMinValue: Int64;
Begin
  Result := 0;
End;

{ UnsignedInt32TypeNode }

function UnsignedInt32Type.GetMaxValue: Int64;
Begin
  Result := 4294967295;
End;

function UnsignedInt32Type.GetMinValue: Int64;
Begin
  Result := 0;
End;

{ UnsignedInt64TypeNode }
function UnsignedInt64Type.GetMaxValue: Int64;
Begin
  Result := 9223372036854775807;
End;

function UnsignedInt64Type.GetMinValue: Int64;
Begin
  Result := -9223372036854775807;
End;

{ SignedInt8TypeNode }

function SignedInt8Type.GetMaxValue: Int64;
Begin
  Result := 127;
End;

function SignedInt8Type.GetMinValue: Int64;
Begin
  Result := -127;
End;

{ SignedInt16TypeNode }

function SignedInt16Type.GetMaxValue: Int64;
Begin
  Result := 32767;
End;

Function SignedInt16Type.GetMinValue: Int64;
Begin
  Result := -32768;
End;

{ SignedInt32TypeNode }
Function SignedInt32Type.GetMaxValue: Int64;
Begin
  Result := 2147483647;
End;

Function SignedInt32Type.GetMinValue: Int64;
Begin
  Result := 2147483647;
End;

{ SignedInt64TypeNode }

function SignedInt64Type.GetMaxValue: Int64;
Begin
  Result := 9223372036854775807;
End;

function SignedInt64Type.GetMinValue: Int64;
Begin
  Result := -9223372036854775807;
End;*)

{ StringTypeNode }
Constructor StringTypeNode.Create(Len: ExpressionNode);
Begin
  Assign(@Self.Length, Len);
End;

{ FixedStringTypeNode }
Constructor FixedStringTypeNode.Create(Expr: ExpressionNode);
Begin
  Assign(@Self.Expr, Expr);
End;

{ RangeTypeNode }
Constructor RangeTypeNode.Create(Min, Max: ExpressionNode);
Begin
  Assign(@Self.Min, Min);
  Assign(@Self.Max, Max);
End;

{ EnumTypeNode }
Constructor EnumTypeNode.Create(enumVals: EnumValueListNode);
Begin
  List := EnumVals;
End;

{ PointerTypeNode }
Constructor PointerTypeNode.Create(PointedType: TypeNode);
Begin
  Self.PointedType := PointedType;
End;

{ FieldDeclarationNode }
Constructor FieldDeclarationNode.FieldDeclaration(Const Name:ASTString; T:TypeNode; IsStatic:Boolean);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, T);
  Self.isStatic := isStatic;
End;

{ PropertySpecifiersNode }

Constructor PropertySpecifiersNode.Create(Const Read, Write: ASTString);
Begin
  Self.Read := Read;
  Self.Write := Write;
End;

Constructor PropertySpecifiersNode.Create(Index: IntLiteralNode; Const Read, Write: ASTString; Stored: ConstExpressionNode; Default: LiteralNode; Const Impl: ASTString);
Begin
  Self.Index := Index;
  Self.Read := Read;
  Self.Write := Write;
  Self.Stored := Stored;
  Self.Default := Default;
  Self.Impl := Impl;
End;

{ PropertyDeclarationNode }
Constructor PropertyDeclarationNode.Create(Const Name:ASTString; T:TypeNode; Specs:PropertySpecifiersNode);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, T);
  Assign(@Self.Specifiers, Specs);
End;

{ ArrayPropertyNode }
Constructor ArrayPropertyNode.Create(Const Name:ASTString; T:TypeNode; Indexes:DeclarationListNode; Specs:PropertySpecifiersNode; Def:Boolean);
Begin
  Self.Name := Name;
  Assign(@Self.DeclType, T);
  Assign(@Self.Indexes, Indexes);
  Assign(@Self.Specifiers, Specs);
  Self.IsDefault := Def;
End;

{ ObjectSectionNode }
Constructor ObjectSectionNode.Create(Fields, Decls:DeclarationListNode; Scope:ScopeEnum);
Begin
  Assign(@Self.Fields, Fields);
  Assign(@Self.Decls, Decls);
  //SCOPE?
End;

Procedure ObjectSectionNode.Add(Sec:ObjectSectionNode);
Begin
  RaiseError('TODO');
End;

procedure ObjectSectionNode.AddDecls(Decls:DeclarationListNode; Scope:ScopeEnum);
Begin
  RaiseError('TODO');
End;

procedure ObjectSectionNode.AddFields(Fields:DeclarationListNode; Scope:ScopeEnum);
Begin
  RaiseError('TODO');
End;

Procedure ObjectSectionNode.AddMethods(Methods:DeclarationListNode; Scope:ScopeEnum);
Begin
  RaiseError('TODO');
End;

procedure ObjectSectionNode.AddProperties(Properties:DeclarationListNode; Scope:ScopeEnum);
Begin
  RaiseError('TODO');
End;

function ObjectSectionNode.GetField(id:AnsiString): FieldDeclarationNode;
Begin
RaiseError('TODO');
End;

function ObjectSectionNode.GetMember(id: AnsiString): DeclarationNode;
Begin
RaiseError('TODO');
End;

function ObjectSectionNode.GetMethod(id: AnsiString): MethodDeclarationNode;
Begin
RaiseError('TODO');
End;

function ObjectSectionNode.GetProperty(id: AnsiString): PropertyDeclarationNode;
Begin
RaiseError('TODO');
End;

{ FunctionAttributeNode }
Constructor FunctionAttributeNode.Create(Value:FunctionAttributeEnum);
Begin
  Self.Value := Value;
End;

{ ClassTypeNode }
Constructor ClassTypeNode.Create(Heritage: IdentifierListNode; Sec: ObjectSectionNode);
Begin
  RaiseError('TODO');
End;

{ ClassRefTypeNode }
Constructor ClassRefTypeNode.Create(reftype: ClassTypeNode);
Begin
  Assign(@Self.RefType, RefType);
End;

Constructor ClassRefTypeNode.Create(Const Name:ASTString; Reftype:ClassTypeNode);
Begin
  Assign(@Self.RefType, RefType);
  Self.QualifID := Name;
End;

{ MetaClassTypeNode }
Constructor MetaClassTypeNode.Create(baseType: TypeNode);
Begin
  Assign(@Self.BaseType, BaseType);
End;

{ IntLiteralNode }
Constructor IntLiteralNode.Create(Value: Int64);
Begin
  Self.Value := Value;
End;

{ CharLiteralNode }
Constructor CharLiteralNode.Create(Value: AnsiChar);
Begin
  Self.Value := Value;
End;

{ BoolLiteralNode }
Constructor BoolLiteralNode.Create(Value: Boolean);
Begin
  Self.Value := Value;
End;

{ StringLiteralNode }

Constructor StringLiteralNode.Create(Const Value:ASTString);
Begin
  Self.Value := Value;
End;

{ RealLiteralNode }

Constructor RealLiteralNode.Create(Value: Double);
Begin
  Self.Value := Value;
End;

{ NullLiteralNode }
Constructor NullLiteralNode.Create();
Begin
End;

{ BinaryExpressionNode }
Constructor BinaryExpressionNode.Create(A, B: ExpressionNode);
Begin
  Assign(@Self.Left, A);
  Assign(@Self.Right, B);
End;

{ InExpressionNode }
Constructor InExpressionNode.Create(A, B: ExpressionNode);
Begin
  Assign(@Self.Expr, A);
  Assign(@Self._Set, B);
End;

{ SetRangeNode }
Constructor SetRangeNode.Create(_type: RangeTypeNode);
Begin
  RaiseError('TODO');
End;

{ TypeBinaryExpressionNode }
Constructor TypeBinaryExpressionNode.Create(Expr:ExpressionNode; ExprType: TypeNode);
Begin
  Assign(@Self.Expr, Expr);
  Assign(@Self.Types, ExprType);
End;

{ SimpleUnaryExpressionNode }
Constructor SimpleUnaryExpressionNode.Create(Expr: ExpressionNode);
Begin
  Assign(@Self.Expr, Expr);
End;

{ SetExpressionNode }
Constructor SetExpressionNode.Create(Elements: ExpressionListNode);
Begin
  Assign(@Self.Elements, Elements);
End;

{ LvalueAsExprNode }
Constructor LvalueAsExprNode.Create(lval: LvalueExpressionNode);
Begin
  Assign(@Self.lval, LVal);
End;

{ ExprAsLvalueNode }
Constructor ExprAsLvalueNode.Create(Expr: ExpressionNode);
Begin
  Assign(@Self.Expr, Expr);
End;

{ StaticCastNode }
Constructor StaticCastNode.Create(T:TypeNode; Expr:ExpressionNode);
Begin
  Assign(@Self.CastType, T);
  Assign(@Self.Expr, Expr);
End;

Constructor StaticCastNode.Create(T:TypeClassNode; Expr:ExpressionNode);
Begin
  Self.CastPrimitive := T;
  Assign(@Self.Expr, Expr);
End;

{ ArrayAccessNode }
Constructor ArrayAccessNode.Create(_array:ArrayConstNode; Acessors:ExpressionListNode);
Begin
  Assign(@Self._Array, _Array);
  Assign(@Self.Acessors, Acessors);
End;

Constructor ArrayAccessNode.Create(lvalue:LvalueExpressionNode; Acessors:ExpressionListNode);
Begin
  Assign(@Self.LValue, LValue);
  Assign(@Self.Acessors, Acessors);
End;

{ PointerDereferenceNode }
Constructor PointerDereferenceNode.Create(Expr: ExpressionNode);
Begin
  Self.Expr := Expr;
End;

{ RoutineCallNode }
Constructor RoutineCallNode.Create(Func: LvalueExpressionNode; RetType: TypeNode);
Begin
  Self.Func := Func;
  Self.ForcedType := RetType;
End;

{ InheritedCallNode }
Constructor InheritedCallNode.Create(Const FuncName:ASTString; Args:ExpressionListNode);
Begin
  Self.FuncName := FuncName;
  Self.Args := Args;
End;

{ ObjectAccessNode }
Constructor ObjectAccessNode.Create(Obj:LvalueExpressionNode; Const Field:ASTString);
Begin
  Self.Obj := Obj;
  Self.Field := Field;
End;

{ IdentifierNode }
Constructor IdentifierNode.Create(Const Name:ASTString; T: TypeNode);
Begin
  Self.Name := Name;
  Self.ForcedType := T;
End;

{ UnresolvedIdNode }
Constructor UnresolvedIdNode.Create(ID: IdentifierNode);
Begin
  Self.ID := ID;
End;

{ UnresolvedCallNode }
Constructor UnresolvedCallNode.Create(lval:LvalueExpressionNode; Args:ExpressionListNode);
Begin
  Self.Func := LVal;
  Self.Args := Args;
End;

{ ConstDeclarationNode }
Constructor ConstDeclarationNode.Create(Const Name:ASTString; Init:ExpressionNode; T:TypeNode);
Begin
  Self.Name := Name;
  Self.Init := Init;
  Self.DeclType := T;
End;

{ EnumValueNode }
Constructor EnumValueNode.Create(Const Name:ASTString; Init:ExpressionNode);
Begin
  Self.Name := Name;
  Self.Init := Init;
End;

{ ProceduralTypeNode }
Constructor ProceduralTypeNode.Create(Params: ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode);
Begin
  Self.Params := Params;
  Self.FuncRet := Ret;
  Self.Directives := Dirs;
End;

{ RoutineDeclarationNode }
Constructor RoutineDeclarationNode.Create(Const Name:ASTString; Params: ParametersSectionNode; Ret: TypeNode; Dirs: FunctionDirectiveListNode);
Begin
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := Ret;
  Self.Directives := Dirs;
End;

{ MethodDeclarationNode }
Constructor MethodDeclarationNode.Create(Const objname, name:ASTString; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Kind:MethodKindEnum);
Begin
  Self.Objname := ObjName;
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := Ret;
  Self.Directives := Dirs;
  Self.Kind := Kind;
End;

{ RoutineDefinitionNode }
Constructor RoutineDefinitionNode.Create(Const Name:ASTString; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Body: RoutineSectionNode);
Begin

End;

Constructor RoutineDefinitionNode.Create(Const Name:ASTString; SignatureType:ProceduralTypeNode; Dirs:FunctionDirectiveListNode; Body:RoutineSectionNode);
Begin
  Self.Name := Name;
  Self.SignatureType := SignatureType;
  Self.Directives := Dirs;
  Self.Body := Body;
End;

{ MethodDefinitionNode }
Constructor MethodDefinitionNode.Create(Const objname, name: ASTString; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Kind: MethodKindEnum; Body:RoutineSectionNode);
Begin
  Self.Objname := ObjName;
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := Ret;
  Self.Directives := Dirs;
  Self.Kind := Kind;
  Self.Body := Body;
End;

{ CompositeDeclarationNode }
Constructor CompositeDeclarationNode.Create(Const Name:ASTString; Ctype:CompositeTypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := Ctype;
End;

{ ExternalDirectiveNode }
Constructor ExternalDirectiveNode.Create(_file, name: ExpressionNode);
Begin
  Self._File := _File;
  Self.Name := Name;
End;

{ ImportDirectivesNode }
Constructor ImportDirectivesNode.Create(ImportDir: FunctionAttributeNode);
Begin
  RaiseError('TODO');
  //Self.
End;

{ LabelStatementNode }
Constructor LabelStatementNode.Create(Const Name:ASTString; Statement:StatementNode);
Begin
  Self.Name := Name;
  Self.Statement := Statement;
End;

{ GotoStatementNode }
Constructor GotoStatementNode.Create(Const LabelName: ASTString);
Begin
  Self.GotoLabel := LabelName;
End;

{ AssignmentNode }
Constructor AssignmentNode.Create(lvalue: LvalueExpressionNode; Expr:ExpressionNode);
Begin
  Self.lvalue := LValue;
  Self.Expr := Expr;
End;

{ IfStatementNode }
Constructor IfStatementNode.Create(condition: ExpressionNode; ifTrue, ifFalse: StatementNode);
Begin
  Self.Condition := Condition;
  Self.ThenBlock := ifTrue;
  Self.ElseBlock := ifFalse;
End;

{ ExpressionStatementNode }
Constructor ExpressionStatementNode.Create(Expr:ExpressionNode);
Begin
  Self.Expr := Expr;
End;

{ CaseSelectorNode }
Constructor CaseSelectorNode.Create(List:ExpressionListNode; Body:StatementNode);
Begin
  Self.List := List;
  Self.Body := Body;
End;

{ CaseStatementNode }
Constructor CaseStatementNode.Create(Condition:ExpressionNode; Selectors:StatementListNode; CaseElse:StatementNode);
Begin
  Self.Condition := Condition;
  Self.Selectors := Selectors;
  Self.CaseElse := CaseElse;
End;

{ LoopStatementNode }

Constructor LoopStatementNode.Create(Body:StatementNode; Condition:ExpressionNode);
Begin
  Self.Body := Body;
  Self.Condition := Condition;
End;

{ ForLoopNode }

Constructor ForLoopNode.Create(_var: IdentifierNode; start, _End:ExpressionNode; Body:StatementNode; Dir:Integer);
Begin
  Self._var := _Var;
  Self.Start := Start;
  Self._End := _End;
  Self.Body := Body;
  Self.Direction := Dir;
End;

{ WithStatementNode }
Constructor WithStatementNode.Create(_with: ExpressionListNode; Body:StatementNode);
Begin
  Self._With := _With;
  Self.Body := Body;
End;

{ TryFinallyStatementNode }
Constructor TryFinallyStatementNode.Create(Body, Final: BlockStatementNode);
Begin
  Self.Body := Body;
  Self.Final := Final;
End;

{ ExceptionBlockNode }
Constructor ExceptionBlockNode.Create(onList:StatementListNode; Default:BlockStatementNode);
Begin
  Self.onList := OnList;
  Self.Default := Default;
End;

{ TryExceptStatementNode }
Constructor TryExceptStatementNode.Create(Body:BlockStatementNode; Final:ExceptionBlockNode);
Begin
  Self.Body := Body;
  Self.Final := Final;
End;

{ RaiseStatementNode }
Constructor RaiseStatementNode.Create(lvalue:LvalueExpressionNode; Expr: ExpressionNode);
Begin
  Self.LValue := LValue;
  Self.Expr := Expr;
End;

{ OnStatementNode }

Constructor OnStatementNode.Create(Const Ident, _type: ASTString; Body:StatementNode);
Begin
  Self.Ident := Ident;
  Self._type := _Type;
  Self.Body := Body;
End;

{ AssemblerBlockNode }
Constructor AssemblerBlockNode.AssemblerBlock(Body:StatementListNode);
Begin
  Self.List := Body;
End;

{ InterfaceTypeNode }
Constructor InterfaceTypeNode.Create(Heritage: IdentifierListNode; Ssec:ObjectSectionNode; guid: StringLiteralNode);
Begin
  Self.Heritage := Heritage;
  Self.Ssec := Ssec;
  Self.Guid := Guid;
End;

{ RecordTypeNode }
Constructor RecordTypeNode.Create(CompTypes:DeclarationListNode);
Begin
  Self.CompTypes := compTypes;
End;

{ ArrayTypeNode }
Constructor ArrayTypeNode.Create(BaseType:TypeNode; Dims:TypeListNode);
Begin
  Self.BaseType := BaseType;
  RaiseError('TODO');
  //Self.Dimensions := Dims;
End;

Constructor ArrayTypeNode.Create(sizeType: TypeNode);
Begin
  RaiseError('TODO');
End;

Constructor ArrayTypeNode.Create(baseType, sizeType: TypeNode);
Begin
  Self.BaseType := BaseType;
  RaiseError('TODO');
End;

Procedure ArrayTypeNode.AddDimension(size: Integer);
Begin
  Inc(DimensionCount);
  SetLength(Dimensions, DimensionCount);
  Dimensions[Pred(DimensionCount)] := Size;
End;

{ SetTypeNode }
Constructor SetTypeNode.Create(T: TypeNode);
Begin
  Self.BaseType := T;
End;

{ FileTypeNode }
Constructor FileTypeNode.Create(T: TypeNode);
Begin
  Self.BaseType := T;
End;

{ VariantDeclarationNode }
Constructor VariantDeclarationNode.Create(Const Name:ASTString; T: TypeNode; Fields: DeclarationListNode);
Begin
  Self.Name := Name;
  Self.DeclType := T;
  Self.Fields := Fields;
End;

{ VarEntryDeclarationNode }
Constructor VarEntryDeclarationNode.Create(TagValue: ExpressionNode; Fields: DeclarationListNode);
Begin
  Self.TagValue := TagValue;
  //Self.Fields := Fields;
  RaiseError('TODO');
End;

{ ExportItemNode }
Constructor ExportItemNode.Create(Const Name:ASTString; pars: ParametersSectionNode; exportname: ASTString);
Begin
  Self.Name := Name;
  Self.FormalParams := Pars;
  Self.ExportName := ExportName;
End;

Constructor ExportItemNode.Create(Const Name:ASTString; pars: ParametersSectionNode; Index: Integer);
Begin
  Self.Name := Name;
  Self.FormalParams := Pars;
  Self.Index := Index;
End;

{ UnresolvedNode }
Constructor UnresolvedNode.Create(Const Name:ASTString);
Begin
  Self.ID := ID;
End;

{ TypeListNode }
Function TypeListNode.GetType(Index: Integer): TypeNode;
Begin
  Result := TypeNode(Self.Get(Index));
End;

{ DeclarationListNode }
Function DeclarationListNode.GetDeclaration(Index: Integer): DeclarationNode;
Begin
  Result := DeclarationNode(Self.Get(Index));
End;

{ UnitListNode }
Function UnitListNode.GetUnit(Index: Integer): UnitItemNode;
Begin
  Result := UnitItemNode(Self.Get(Index));
End;

{ StatementListNode }
Constructor StatementListNode.Create(St: StatementNode);
Begin
  Assign(@Self.List, ListNode.Create(St));
End;

Procedure StatementListNode.Add(St: StatementNode);
Begin
  Self.List.Add(St);
End;

Function StatementListNode.GetStatement(Index: Integer): StatementNode;
Begin
  Result := StatementNode(List.Get(Index));
End;


{ ExpressionListNode }
Function ExpressionListNode.GetExpression(Index: Integer): ExpressionNode;
Begin
  Result := ExpressionNode(Self.Get(Index));
End;

{ EnumValueListNode }
Function EnumValueListNode.GetEnum(Index: Integer): EnumValueNode;
Begin
  Result := EnumValueNode(Self.Get(Index));
End;

{ IdentifierListNode }
Function IdentifierListNode.GetIdentifier(Index: Integer): IdentifierNode;
Begin
  Result := IdentifierNode(Self.Get(Index));
End;

End.
