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
Uses TypInfo, TERRA_Collections;

Type
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


  ASTNode = Class
    Protected
      Procedure LinkChild(Child:ASTNode);

    Published
      Owner:ASTNode;
  End;

  ListNode = Class(ASTNode)
    Elements:Array Of ASTNode;
    Count:Integer;

    Constructor Create(Element:ASTNode = Nil);

    Procedure Add(Element:ASTNode); Overload;
    Procedure Add(List:ListNode); Overload;

    Procedure InsertAt(Index:Integer; Element:ASTNode); Overload;
    Procedure InsertAt(Index:Integer; List:ListNode); Overload;

    Function Get(Index:Integer):ASTNode;
  End;

	SourceNode = Class(ASTNode)
    Protected
      Name:StringObject;
  End;

  TypeNode = Class(ASTNode)
    Protected
  		// TODO each derive should set the typesize
	  	TypeSize:Integer;
  End;

  TypeListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):TypeNode;
  End;

  MetaTypeClass = Class Of TypeNode;

  MetaTypeNode = Class(TypeNode)
    Value:MetaTypeClass;

    Constructor Create(TypeClass:MetaTypeClass);
  End;

  DeclarationNode = Class(ASTNode)
		Name:StringObject;
		DeclType:TypeNode;

		Constructor Create(Name:StringObject; T:TypeNode = Nil);
  End;

  DeclarationListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):DeclarationNode;
  End;

  UnitItemNode = Class(DeclarationNode)
    Location:StringObject;

    Constructor Create(Name:StringObject; Location:StringObject); Overload;
    Constructor Create(Name:StringObject); Overload;
  End;

  UnitListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):UnitItemNode;
  End;

	StatementNode = Class(ASTNode)
  End;

  CompositeDeclarationNode = Class;
  IdentifierListNode = Class;

	SectionNode = Class(ASTNode)
		Decls:DeclarationListNode;

		Constructor Create(Decls:DeclarationListNode);
  End;

  TopLevelDeclarationSectionNode = Class(SectionNode)
    Useslist:UnitListNode;

		Constructor Create(UsesList:UnitListNode; Decls:DeclarationListNode);
  End;


	InterfaceSectionNode = Class(TopLevelDeclarationSectionNode)
  End;

	ImplementationSectionNode = Class(TopLevelDeclarationSectionNode)
  End;

  StatementListNode = Class(StatementNode)
    Protected
      List:ListNode;

    Public
      Constructor Create(St:StatementNode);
      Procedure Add(St:StatementNode);

      Function Get(Index:Integer):StatementNode;
  End;

	BlockStatementNode = Class(StatementNode)
		List:StatementListNode;

		Constructor Create(List:StatementListNode);
  End;

	ProgramSectionNode = Class(TopLevelDeclarationSectionNode)
    Block:BlockStatementNode;

    Constructor Create(UsesList:UnitListNode; Decls:DeclarationListNode; Code:BlockStatementNode);
  End;

	ProgramNode = Class(SourceNode)
    Section:ProgramSectionNode;

    Constructor Create(Name:StringObject; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
  End;

	ConstantValueNode = Class(ASTNode)
  End;

  ExpressionNode = Class(ASTNode)
		ForcedType:TypeNode;
    EnforceConst:Boolean;
  End;

  ExpressionListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):ExpressionNode;
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
		Init:ExpressionNode;
		AbsoluteID:StringObject;
		IsThrVar:Boolean;

		Constructor Create(Name:StringObject; VarType:TypeNode; Init:ExpressionNode = Nil; AbsoluteId:StringObject = Nil);
  End;

	/// Routine parameters. May be value (default), variable, constant, or out.
	/// Param types must be an id, string or open array (array of paramtype)
	ParamDeclarationNode = Class(ValueDeclarationNode)
		Init:ExpressionNode;
    Kind:ParameterKindEnum;

		Constructor Create(Name:StringObject; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum);
  End;

	VarParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Name:StringObject; VarType:TypeNode);
  End;

	ConstParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Name:StringObject; ConstType:TypeNode; Init:ExpressionNode = Nil);
  End;

	OutParamDeclarationNode = Class(ParamDeclarationNode)
		Constructor Create(Name:StringObject; VarType:TypeNode);
  End;


  ParametersSectionNode = Class(SectionNode)
		  ReturnVar:ParamDeclarationNode; //TODO

		  Constructor Create(Decls:DeclarationListNode=Nil);
		//public override bool Equals(object obj)
		{
			ParametersSection sec = obj as ParametersSection;
			return sec != null && returnVar.Equals(sec.returnVar) && decls.SequenceEqual(sec.decls);
		}
  End;


	/// Directives constraints:
	///		Override | Abstract
	///		Abstract => virtual
	///		varargs => cdecl
  FunctionAttributeNode = Class(ASTNode)
    Value:FunctionAttributeEnum;

    Constructor Create(Value:FunctionAttributeEnum);
  End;

	FunctionDirectiveListNode  = Class(ListNode)
		CallConv:FunctionAttributeNode;

		/// Checks the immediate coherence between function directives.
		/// Must be called after all directives are added
		Function CheckDirectives():Boolean;
  End;

  ProceduralTypeNode = Class;

	/// Declaration of a Callable unit, i.e. a global routine or method
	CallableDeclarationNode = Class(DeclarationNode)
		/// Gets the fully qualified name of this callable
		/// (obj+metname for methods, plus parameter types for overloads)
		/// To be set by the Resolver
		QualifiedName:StringObject;

		/// Section that declares this callable.
		/// To be set by resolver
		DeclaringSection:SectionNode;

    SignatureType:ProceduralTypeNode;
    ResultType:TypeNode;

    Directives:FunctionDirectiveListNode;

		Constructor Create(Name:StringObject; Params:ParametersSectionNode; RetType:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);
  End;


	RoutineSectionNode = Class(SectionNode)
    Block:StatementNode;

		 // to be set by resolver
		DeclaringCallable:CallableDeclarationNode;

		Constructor Create(Decls:DeclarationListNode; Block:StatementNode);
  End;

	LibraryNode = Class(SourceNode)
		Section:ProgramSectionNode;

		Constructor Create(Name:StringObject; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
  End;

	UnitNode = Class(SourceNode)
		Interfaces:InterfaceSectionNode;
		Implements:ImplementationSectionNode;
		Inits:BlockStatementNode;
		Final:BlockStatementNode;

		Constructor Create(Name:StringObject; Interfce:InterfaceSectionNode; Impl:ImplementationSectionNode;  Init:BlockStatementNode = Nil; Final:BlockStatementNode = Nil);
  End;

  PackageNode = Class(SourceNode)
		Requires:UnitListNode;
		Contains:UnitListNode;

		Constructor Create(Name:StringObject; requires, contains:UnitListNode);
  End;

  StructuredConstantNode = Class(ConstExpressionNode)
    ExprList:ExpressionListNode;

		Constructor Create(ExprList:ExpressionListNode);
  End;

	ArrayConstNode = Class(StructuredConstantNode)
    Constructor Create(ExprList:ExpressionListNode); Overload;
		Constructor Create(ArrayElems:StringObject); Overload;
			//: base(new ExpressionList(arrayElems.ToCharArray().Select(x => new CharLiteral(x)))){ }
	End;

  FieldInitNode = Class(ConstExpressionNode)
	  FieldName:StringObject;
    Expr:ExpressionNode;

    Constructor Create(Name:StringObject; Expr:ExpressionNode);
  End;

  FieldInitListNode = Class(ExpressionListNode)
    Constructor Create(F:FieldInitNode = Nil);
    //Procedure Add(F:FieldInit);
  End;

  RecordConstNode = Class(StructuredConstantNode)
    Constructor Create(ExprList:FieldInitListNode);
  End;

  ConstIdentifierNode = Class(ConstExpressionNode)
    Name:StringObject;

    Constructor Create(Name:StringObject);
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
    Length:ExpressionNode;

    Constructor Create(Len:ExpressionNode = Nil);
	End;

	FixedStringTypeNode = Class(StringTypeNode)
		Expr:ExpressionNode;
		Len:Integer;

		Constructor Create(Expr:ExpressionNode);
  End;

	RangeTypeNode = Class(TypeNode)
		Min:ExpressionNode;
		Max:ExpressionNode;

		Constructor Create(Min, Max:ExpressionNode);
  End;

  EnumValueNode = Class;
  EnumValueListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):EnumValueNode;
  End;

	EnumTypeNode = Class(TypeNode)
    List:EnumValueListNode;

		Constructor Create(EnumVals:EnumValueListNode);
  End;

	/// Variants can hold values of any type except records, sets, static arrays, files, classes, class references, and pointers.
	/// I.e. can hold anything but structured types and pointers.
	/// They can hold interfaces, dynamic arrays, variant arrays
	VariantTypeNode = Class(TypeNode)
		actualtype:TypeNode ;
  End;

	/// PointedType may be any type.
	/// It may be a not yet declared type (forward declaration)
	PointerTypeNode = Class(ScalarTypeNode)
		PointedType:TypeNode;

		Constructor Create(PointedType:TypeNode);
  End;


	PropertySpecifiersNode = Class(ASTNode)
		Public
      Index:IntLiteralNode;
		  Read:StringObject;
		  Write:StringObject;
      Stored:ConstExpressionNode;
		  Default:LiteralNode;	// nodefault == Int32.MaxValue
		  Impl:StringObject;

		  Constructor Create(Read, Write:StringObject); Overload;
		  Constructor Create(Index:IntLiteralNode; Read, Write:StringObject; Stored:ConstExpressionNode; Default:LiteralNode; Impl:StringObject = Nil); Overload;
  End;


  ObjectSectionNode = Class;
  CompositeTypeNode = Class(TypeNode)
		Heritage:IdentifierListNode;

		Section:ObjectSectionNode;

		Decl:CompositeDeclarationNode;

		IsPacked:Boolean;

		// optional
		Name:StringObject;


    Ancestors:Array Of CompositeTypeNode;
    AncestorCount:Integer;

		//public List<CompositeType> ancestors;

		Function IsForward:Boolean;
  End;

	FieldDeclarationNode = Class(ValueDeclarationNode)
		isStatic:Boolean;
		Scope:ScopeEnum;
		DeclaringObject:CompositeTypeNode;

		Constructor FieldDeclaration(Id:StringObject; T:TypeNode = Nil; IsStatic:Boolean = False);
  End;

	PropertyDeclarationNode = Class(FieldDeclarationNode)
    Public
  		Specifiers:PropertySpecifiersNode;

		  IsStatic:Boolean;

		  Constructor Create(ID:StringObject; T:TypeNode; Specs:PropertySpecifiersNode = Nil);
  End;

	ProceduralTypeNode = Class(TypeNode)
		Params:ParametersSectionNode;

		/// Function's return type. Must be null for every non-function routine.
		FuncRet:TypeNode ;

		Directives:FunctionDirectiveListNode ;

		Constructor Create(Params:ParametersSectionNode; ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);
  End;

	MethodTypeNode = Class(ProceduralTypeNode)
		Kind:MethodKindEnum;
  End;

	/// Declaration of a Method
	MethodDeclarationNode = Class(CallableDeclarationNode)
		isStatic:Boolean;
		Objname:StringObject;
		Name:StringObject;
    Scope:ScopeEnum;
		DeclaringObject:CompositeTypeNode;
    Kind:MethodKindEnum;

		Constructor Create(objname:StringObject; name:StringObject; params:ParametersSectionNode; ret:TypeNode = Nil; dirs:FunctionDirectiveListNode  = Nil; kind:MethodKindEnum  = method_Default);
	End;

	ObjectSectionNode = Class(SectionNode)
		Fields:DeclarationListNode;
		Properties:DeclarationListNode;
    //Methods:DeclarationList;
		DeclaringObject:CompositeTypeNode;

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
  End;


	ArrayPropertyNode = Class(PropertyDeclarationNode)
		Indexes:DeclarationListNode;
		IsDefault:Boolean;

		Constructor Create(Id:StringObject; T:TypeNode; Indexes:DeclarationListNode; Specs:PropertySpecifiersNode; Def:Boolean);
	End;

  ClassTypeNode = Class(CompositeTypeNode)
		_self:FieldDeclarationNode;

    Public
		  Constructor Create(Heritage:IdentifierListNode; Sec:ObjectSectionNode  = Nil);
  End;

	ClassRefTypeNode = Class(ClassTypeNode)
		QualifID:StringObject;
		RefType:ClassTypeNode;

		Constructor Create(reftype:ClassTypeNode); Overload;
		Constructor Create(qualifid:StringObject; reftype:ClassTypeNode = Nil); Overload;
  End;

	MetaClassTypeNode = Class(ScalarTypeNode)
		BaseType:TypeNode;

		Constructor Create(baseType:TypeNode);
  End;

  LiteralNode = Class(ConstExpressionNode)
    //Constructor Create(Val:ConstantValue; T:TypeNode);
  End;

	OrdinalLiteralNode = Class(LiteralNode)
    //Constructor Create(V:Cardinal; T:IntegralType);
  End;

	IntLiteralNode = Class(OrdinalLiteralNode)
    Value:Int64;

    Constructor Create(Value:Int64);
  End;

	CharLiteralNode = Class(OrdinalLiteralNode)
    Value:AnsiChar;

    Constructor Create(Value:AnsiChar);
  End;

	BoolLiteralNode = Class(OrdinalLiteralNode)
    Value:Boolean;

    Constructor Create(Value:Boolean);
  End;

	StringLiteralNode = Class(LiteralNode)
    Value:StringObject;

    Constructor Create(Value:StringObject);
  End;

	RealLiteralNode = Class(LiteralNode)
    Value:Double;

    Constructor Create(Value:Double);
  End;

	NullLiteralNode = Class(LiteralNode)
    Constructor Create();
  End;

	BinaryExpressionNode = Class(ExpressionNode)
		Left:ExpressionNode;
		Right:ExpressionNode;

    Constructor Create(A, B:ExpressionNode);
  End;

  // eg: if (A in B) then
	InExpressionNode = Class(BinaryExpressionNode)
		Expr:ExpressionNode;
		_Set:ExpressionNode;		// enforce that 'set' is in fact a set

		Constructor Create(A, B:ExpressionNode);
  End;

	SetRangeNode = Class(BinaryExpressionNode)
    Range:RangeTypeNode;

		Constructor Create(_type:RangeTypeNode);
		{
			this.ForcedType = this.Type = type;
			this.EnforceConst = true;
		}
  End;

	ArithmeticBinaryExpressionNode  = Class(BinaryExpressionNode)
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
		  op:LogicalBinaryOp;
  End;

	ComparisonBinaryExpressionNode = Class(BinaryExpressionNode)
		op:ComparisonBinaryOp;
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
		Expr:ExpressionNode;
		Types:TypeNode;

		Constructor Create(Expr:ExpressionNode; ExprType:TypeNode);
  End;

  // eg: if A is B Then
	IsExpressionNode = Class(TypeBinaryExpressionNode)
  End;

	RuntimeCastNode = Class(TypeBinaryExpressionNode)
  End;

	UnaryExpressionNode = Class(ExpressionNode)
  End;

	SimpleUnaryExpressionNode = Class(ExpressionNode)
		Expr:ExpressionNode;

		Constructor Create(Expr:ExpressionNode);
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
		Elements:ExpressionListNode;

		Constructor Create(Elements:ExpressionListNode = Nil);
  End;

//Pretty simply, an rvalue is when the expression result will not survive past the end of said expression. An lvalue will.
// This basic principle is what enables move semantics and rvalue references- that you can modify them without issue, because you know that object's life is over.  
  LvalueExpressionNode = Class;

	/// Cast an lvalue to an rvalue (Expr)
	LValueAsExprNode = Class(UnaryExpressionNode)
		lval:LvalueExpressionNode;

		Constructor Create(lval:LvalueExpressionNode);
  End;

	LvalueExpressionNode = Class(UnaryExpressionNode)
  End;

	/// Cast an rvalue (Expr) to an lvalue
	ExprAsLvalueNode = Class(LvalueExpressionNode)
		Expr:ExpressionNode;

		Constructor Create(Expr:ExpressionNode);
  End;

	/// eg: VarType(expr)
	StaticCastNode = Class(LvalueExpressionNode)
		CastType:TypeNode;
    CastPrimitive:TypeClassNode;
		Expr:ExpressionNode;

		Constructor Create(T:TypeNode; Expr:ExpressionNode); Overload;
    Constructor Create(T:TypeClassNode; Expr:ExpressionNode); Overload;
  End;

	ArrayAccessNode = Class(LvalueExpressionNode)
		LValue:LvalueExpressionNode;
		Acessors:ExpressionListNode;
		// object alternative to lvalue
		_Array:ArrayConstNode;

		Constructor Create(_array:ArrayConstNode; Acessors:ExpressionListNode); Overload;
		Constructor Create(lvalue:LvalueExpressionNode; Acessors:ExpressionListNode); Overload;
  End;

	PointerDereferenceNode = Class(LvalueExpressionNode)
		Expr:ExpressionNode;

		Constructor Create(Expr:ExpressionNode);
  End;

	RoutineCallNode = Class(LvalueExpressionNode)
		Func:LvalueExpressionNode;
		Args:ExpressionListNode;

		Constructor Create(Func:LvalueExpressionNode; RetType:TypeNode = Nil);
	//RoutineCall(LvalueExpression func, ExpressionList args, TypeNode retType = null)
  End;

	InheritedCallNode = Class(RoutineCallNode)
		FuncName:StringObject;
		// to be set by resolver
		DeclaringObject:CompositeTypeNode;

		Constructor Create(FuncName:StringObject; Args:ExpressionListNode =Nil);
  End;

	/// An access to a member in an object (record, class or interface)
	ObjectAccessNode = Class(LvalueExpressionNode)
		Obj:LvalueExpressionNode;
		Field:StringObject;

		Constructor Create(Obj:LvalueExpressionNode; Field:StringObject);
  End;

	/// Identifier that refers to a named declaration
	IdentifierNode = Class(LvalueExpressionNode)
    Name:StringObject;
		Decl:DeclarationNode;

		Constructor Create(Name:StringObject; T:TypeNode = Nil);
  End;

  IdentifierListNode = Class(ListNode)
    Public
      Function Get(Index:Integer):IdentifierNode;
  End;

	/// Identifier that refers to a named class. Static access
	IdentifierStaticNode = Class(IdentifierNode)
  End;

	UnresolvedLvalueNode = Class(LValueExpressionNode)
	End;

	UnresolvedIdNode = Class(UnresolvedLvalueNode)
		ID:IdentifierNode;

		Constructor Create(ID:IdentifierNode);
  End;

	/// Call, to be resolver after parsing
	UnresolvedCallNode = Class(UnresolvedLvalueNode)
		Func:LvalueExpressionNode;
		Args:ExpressionListNode;

		Constructor Create(lval:LvalueExpressionNode; Args:ExpressionListNode = Nil);
  End;


	/// TODO!! Must Derive type
	ConstDeclarationNode = Class(ValueDeclarationNode)
		Init:ExpressionNode;

		Constructor Create(Name:StringObject; Init:ExpressionNode; T:TypeNode = Nil);
  End;

	EnumValueNode = Class(ConstDeclarationNode)
    Constructor Create(Name:StringObject; Init:ExpressionNode = Nil);
  End;

	/// Creates a custom, user-defined name for some Type
	TypeDeclarationNode  = Class(DeclarationNode)
  End;

	/// Declaration of a global Routine
	RoutineDeclarationNode = Class(CallableDeclarationNode)
		Constructor Create(Name:StringObject; Params:ParametersSectionNode; Ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil);
  End;

	/// Routine definition (implementation)
	RoutineDefinitionNode = Class(RoutineDeclarationNode)
		Body:RoutineSectionNode;

		Constructor Create(name:StringObject; params:ParametersSectionNode; ret:TypeNode = Nil; dirs:FunctionDirectiveListNode = Nil; body:RoutineSectionNode = Nil); Overload;
    Constructor Create(name:StringObject; signatureType:ProceduralTypeNode; dirs:FunctionDirectiveListNode = Nil; body:RoutineSectionNode = Nil); Overload;
  End;

	/// Method definition (implementation)
	MethodDefinitionNode = Class(MethodDeclarationNode)
		Body:RoutineSectionNode;

		Constructor Create(objname:StringObject; Name:StringObject; Params:ParametersSectionNode; Ret:TypeNode = Nil; Dirs:FunctionDirectiveListNode = Nil; Kind:MethodKindEnum  = Method_Default; Body:RoutineSectionNode = Nil);
  End;

	CompositeDeclarationNode = Class(TypeDeclarationNode)

		Constructor Create(Name:StringObject; ctype:CompositeTypeNode);
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
		_File:ExpressionNode;
		Name:ExpressionNode;

		Constructor Create(_file:ExpressionNode; Name:ExpressionNode = Nil);
  End;

	ImportDirectivesNode = Class(FunctionDirectiveListNode)
		ImportDir:FunctionAttributeNode;
		_external:ExternalDirectiveNode;

    Constructor Create(ImportDir:FunctionAttributeNode);
  End;

	LabelStatementNode = Class(StatementNode)
		Name:StringObject;
		Statement:StatementNode;

		// to be set by the resolver
		Decl:LabelDeclarationNode;

		Constructor Create(Name:StringObject; Statement:StatementNode);
  End;

	GotoStatementNode = Class(StatementNode)
		GotoLabel:StringObject;

		// to be set by the resolver
		Decl:LabelDeclarationNode;

		Constructor Create(LabelName:StringObject);
  End;

	EmptyStatementNode = Class(StatementNode)
  End;

	BreakStatementNode = Class(StatementNode)
  End;

	ContinueStatementNode = Class(StatementNode)
  End;

	AssignmentNode = Class(StatementNode)
		lvalue:LvalueExpressionNode;
		Expr:ExpressionNode;

		Constructor Create(lvalue:LvalueExpressionNode; Expr:ExpressionNode);
  End;

	IfStatementNode = Class(StatementNode)
		Condition:ExpressionNode;
		ThenBlock:StatementNode;
		ElseBlock:StatementNode;

		Constructor Create(condition:ExpressionNode ; ifTrue:StatementNode; ifFalse:StatementNode=Nil);
  End;

	ExpressionStatementNode = Class(StatementNode)
		Expr:ExpressionNode;

		Constructor Create(Expr:ExpressionNode);
  End;

	CaseSelectorNode = Class(StatementNode)
		List:ExpressionListNode;
		Body:StatementNode;

		Constructor Create(List:ExpressionListNode; Body:StatementNode);
  End;

	CaseStatementNode = Class(StatementNode)
		Condition:ExpressionNode;
		Selectors:StatementListNode;
		CaseElse:StatementNode;

		Constructor Create(Condition:ExpressionNode; Selectors:StatementListNode; CaseElse:StatementNode);
  End;

	LoopStatementNode = Class(StatementNode)
		Condition:ExpressionNode;
		Body:StatementNode;

		Constructor Create(Body:StatementNode; Condition:ExpressionNode);
  End;

	RepeatLoopNode = Class(LoopStatementNode)
  End;

	WhileLoopNode = Class(LoopStatementNode)
  End;

	ForLoopNode = Class(LoopStatementNode)
		_var:IdentifierNode;
		Start:ExpressionNode;
		_End:ExpressionNode;
		Direction:Integer;

		Constructor Create(_var:IdentifierNode; start:ExpressionNode; _End:ExpressionNode; Body:StatementNode; Dir:Integer);
  End;

	WithStatementNode = Class(StatementNode)
		_With:ExpressionListNode;
		Body:StatementNode;

		Constructor Create(_with:ExpressionListNode; Body:StatementNode);
  End;

	TryFinallyStatementNode = Class(StatementNode)
		Body:BlockStatementNode;
		Final:BlockStatementNode;

		Constructor Create(Body, Final:BlockStatementNode);
  End;

	ExceptionBlockNode = Class(StatementNode)
  	onList:StatementListNode;
		Default:BlockStatementNode;	// else or default, same semantics

		Constructor Create(onList:StatementListNode; Default:BlockStatementNode = Nil);
  End;

	TryExceptStatementNode = Class(StatementNode)
		Body:BlockStatementNode;
		Final:ExceptionBlockNode;

		Constructor Create(Body:BlockStatementNode; Final:ExceptionBlockNode);
  End;


	RaiseStatementNode = Class(StatementNode)
		LValue:LvalueExpressionNode;
		Expr:ExpressionNode;

		Constructor Create(lvalue:LvalueExpressionNode; Expr:ExpressionNode);
  End;

	OnStatementNode = Class(StatementNode)
		Ident:StringObject;
		_type:StringObject;
		Body:StatementNode;

		Constructor Create(Ident, _type:StringObject; Body:StatementNode);
  End;

	AssemblerBlockNode = Class(BlockStatementNode)
		Constructor AssemblerBlock(Body:StatementListNode);
  End;

	InterfaceTypeNode = Class(CompositeTypeNode)
    Ssec:ObjectSectionNode;
		Guid:StringLiteralNode;

		Constructor Create(Heritage:IdentifierListNode; Ssec:ObjectSectionNode = Nil; guid:StringLiteralNode  = Nil);
  End;


	///			StructuredType > Type
	///				Array > Type
	///				Set	  > OrdinalType
	///				File  > VariableType
	///		 		Record > TypeNode ...

	StructuredTypeNode = Class(TypeNode)
		BaseType:TypeNode;
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
  End;

	RecordTypeNode = Class(StructuredTypeNode)
		CompTypes:DeclarationListNode;

		Constructor Create(compTypes:DeclarationListNode);
  End;

	RecordFieldDeclarationNode = Class(ValueDeclarationNode)
  End;

  ArrayTypeNode = Class(StructuredTypeNode)
  	Dimensions:Array Of Integer;
    DimensionCount:Integer;

  	Constructor Create(baseType:TypeNode; Dims:TypeListNode); Overload;
	  Constructor Create(baseType, SizeType:TypeNode); Overload;
    Constructor Create(sizeType:TypeNode); Overload;

  	Procedure AddDimension(size:Integer);
  End;

  SetTypeNode = Class(StructuredTypeNode)
    Constructor Create(T:TypeNode);
  End;

  FileTypeNode = Class(StructuredTypeNode)
    Constructor Create(T:TypeNode);
  End;

	VariantDeclarationNode = Class(RecordFieldDeclarationNode)
		Fields:DeclarationListNode;

		Constructor Create(ID:StringObject; T:TypeNode; Fields:DeclarationListNode);
  End;

	/// Variant case entry declaration
	VarEntryDeclarationNode = Class(RecordFieldDeclarationNode)
		TagValue:ExpressionNode;
		Fields:RecordTypeNode;

		Constructor Create(TagValue:ExpressionNode; Fields:DeclarationListNode);	// type must be later set to the variant type
	End;

  ExportItemNode = Class(UnitItemNode)
		FormalParams:ParametersSectionNode;
		ExportName:StringObject;
		Index:Integer;

		Constructor Create(Name:StringObject; pars:ParametersSectionNode; ExportName:StringObject = Nil); Overload;
    Constructor Create(Name:StringObject; pars:ParametersSectionNode; Index:Integer); Overload;
  End;

	UnresolvedNode = Class(TypeNode)
		ID:StringObject;

		Constructor Create(ID:StringObject);
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
Procedure ASTNode.LinkChild(Child: ASTNode);
Var
  Count, I: Integer;
  List: TPropList;
  PropName:AnsiString;
Begin
  Child.Owner := Self;
  // loop through all node published properties
  (*Count := GetPropList(Child.ClassInfo, tkAny, @List);
  For I:=0 To Pred(Count) Do
  Begin
    PropName := List[I].Name;

    If (PropName = 'Owner') Then
    Begin
      List[I].SetProc
    End;
  End;*)
End;


{ MetaType }
Constructor MetaTypeNode.Create(TypeClass: MetaTypeClass);
Begin
  Self.Value := TypeClass;
End;

{ Declaration }
Constructor DeclarationNode.Create(Name: StringObject; T: TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := T;
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

  Inc(Count);
  SetLength(Elements, Count);
  Elements[Pred(Count)] := Element;

  LinkChild(Element);
End;

Procedure ListNode.Add(List: ListNode);
Var
  I:Integer;
Begin
  If List = Nil Then
    Exit;

  For I:=0 To Pred(List.Count) Do
    Self.Add(List.Elements[I]);
End;

Procedure ListNode.InsertAt(Index:Integer; List:ListNode);
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
    Elements[I+Prev] := List.Elements[I];
End;

Procedure ListNode.InsertAt(Index:Integer; Element:ASTNode);
Var
  I:Integer;
Begin
  If List = Nil Then
    Exit;

  If (Index>=Count) Then
  Begin
    Self.Add(Element);
    Exit;
  End;

  Inc(Count);
  SetLength(Elements, Count);

  For I:=Index To (Count - 2) Do
    Elements[I + 1] := Elements[I];

  Elements[I+Pred(Count)] := Element;
End;

Function ListNode.Get(Index: Integer): ASTNode;
Begin
  If (Index<0) Or (Index>=Count) Then
    Result := Nil
  Else
    Result := Elements[Index];
End;

{ UnitItemNode }
Constructor UnitItemNode.Create(Name, Location: StringObject);
Begin
  Self.Name := Name;
  Self.Location := Location;
  Self.DeclType := Nil;
End;

Constructor UnitItemNode.Create(Name: StringObject);
Begin
  Self.Create(Name, Nil);
End;

{ ProgramNode }
Constructor ProgramNode.Create(Name:StringObject; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
Begin
  Self.Name := Name;
  Self.Section := ProgramSectionNode.Create(UsesList, Decls, Body);
End;

{ CompositeTypeNode }
Function CompositeTypeNode.IsForward: Boolean;
Begin
  Result := (Section = Nil);
End;

{ SectionNode }
Constructor SectionNode.Create(Decls: DeclarationListNode);
Begin
  Self.Decls := Decls;
End;

{ TopLevelDeclarationSectionNode }
Constructor TopLevelDeclarationSectionNode.Create(UsesList:UnitListNode; Decls:DeclarationListNode);
Begin
  Self.Useslist := UsesList;
  Self.Decls := Decls;
End;

{ BlockStatementNode }
Constructor BlockStatementNode.Create(List: StatementListNode);
Begin
  Self.List := List;
End;

{ ProgramSectionNode }
Constructor ProgramSectionNode.Create(UsesList: UnitListNode; Decls: DeclarationListNode; Code: BlockStatementNode);
Begin
  Self.Block := Code;
  Self.Useslist := UsesList;
  Self.Decls := Decls;
End;

{ ConstExpressionNode }
Function ConstExpressionNode.ResolveToLiteral: LiteralNode;
Begin
  Result := Nil; //TODO
End;

{ VarDeclarationNode }
Constructor VarDeclarationNode.Create(Name:StringObject; VarType:TypeNode; Init:ExpressionNode; AbsoluteId:StringObject);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;
  Self.Init := Init;
  Self.AbsoluteID := AbsoluteID;

  LinkChild(VarType);
End;


{ ParamDeclarationNode }
Constructor ParamDeclarationNode.Create(Name:StringObject; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum);
Begin
  Self.Name := Name;
  Self.DeclType := ParamType;
  Self.Init := Init;
  Self.Kind := Kind;

  LinkChild(ParamType);
End;

{ VarParamDeclarationNode }
Constructor VarParamDeclarationNode.Create(Name:StringObject; VarType:TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;

  LinkChild(VarType);
End;

{ ConstParamDeclarationNode }
Constructor ConstParamDeclarationNode.Create(Name:StringObject; ConstType:TypeNode; Init:ExpressionNode);
Begin
  Self.Name := Name;
  Self.DeclType := ConstType;
  Self.Init := Init;

  LinkChild(ConstType);
End;

{ OutParamDeclarationNode }
Constructor OutParamDeclarationNode.Create(Name:StringObject; VarType:TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;

  LinkChild(VarType);
End;

{ ParametersSectionNode }
Constructor ParametersSectionNode.Create(Decls: DeclarationListNode);
Begin
  Self.Decls := Decls;
End;

{ FunctionDirectiveListNode }
Function FunctionDirectiveListNode.CheckDirectives: Boolean;
Begin
  // TODO
End;

{ CallableDeclarationNode }

Constructor CallableDeclarationNode.Create(Name: StringObject; Params: ParametersSectionNode; RetType:TypeNode; Dirs: FunctionDirectiveListNode);
Begin
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := RetType;
  Self.Directives := Dirs;
End;

{ RoutineSectionNode }

Constructor RoutineSectionNode.Create(Decls: DeclarationListNode; Block: StatementNode);
Begin
  Self.Decls := Decls;
  Self.Block := Block;
End;

{ LibraryNode }
Constructor LibraryNode.Create(Name:StringObject; UsesList:UnitListNode; Decls:DeclarationListNode; Body:BlockStatementNode);
Begin
  Self.Name := Name;
  Self.Section := ProgramSectionNode.Create(UsesList, Decls, Body);
End;

{ UnitNode }
Constructor UnitNode.Create(Name: StringObject; Interfce:InterfaceSectionNode; Impl:ImplementationSectionNode; Init, Final: BlockStatementNode);
Begin
  Self.Name := Name;
  Self.Interfaces := Interfce;
  Self.Implements := Impl;
  Self.Inits := Init;
  Self.Final := Final;
End;

{ PackageNode }
Constructor PackageNode.Create(Name: StringObject; Requires, Contains: UnitListNode);
Begin
  Self.Name := Name;
  Self.Requires := Requires;
  Self.Contains := Contains;
End;

{ StructuredConstantNode }

Constructor StructuredConstantNode.Create(ExprList: ExpressionListNode);
Begin
  Self.ExprList := ExprList;
End;

{ ArrayConstNode }
Constructor ArrayConstNode.Create(ExprList: ExpressionListNode);
Begin
  Self.ExprList := ExprList;
End;

Constructor ArrayConstNode.Create(ArrayElems: StringObject);
Begin
  RaiseError('TODO');
End;

{ FieldInitNode }
Constructor FieldInitNode.Create(Name: StringObject; Expr: ExpressionNode);
Begin
  Self.FieldName := Name;
  Self.Expr := Expr;
End;

{ FieldInitListNode }
Constructor FieldInitListNode.Create(F: FieldInitNode);
Begin
  RaiseError('TODO');
End;

{ RecordConstNode }
Constructor RecordConstNode.Create(ExprList: FieldInitListNode);
Begin
  Self.ExprList := ExprList;
End;

{ ConstIdentifierNode }

Constructor ConstIdentifierNode.Create(Name: StringObject);
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
  Self.Length := Len;
End;

{ FixedStringTypeNode }
Constructor FixedStringTypeNode.Create(Expr: ExpressionNode);
Begin
  Self.Expr := Expr;
End;

{ RangeTypeNode }
Constructor RangeTypeNode.Create(Min, Max: ExpressionNode);
Begin
  Self.Min := Min;
  Self.Max := Max;
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
Constructor FieldDeclarationNode.FieldDeclaration(Id: StringObject; T: TypeNode; IsStatic: Boolean);
Begin
  Self.Name := Id;
  Self.DeclType := T;
  Self.isStatic := isStatic;
End;

{ PropertySpecifiersNode }

Constructor PropertySpecifiersNode.Create(Read, Write: StringObject);
Begin

End;

Constructor PropertySpecifiersNode.Create(Index: IntLiteralNode; Read, Write: StringObject; Stored: ConstExpressionNode; Default: LiteralNode; Impl: StringObject);
Begin
  Self.Index := Index;
  Self.Read := Read;
  Self.Write := Write;
  Self.Stored := Stored;
  Self.Default := Default;
  Self.Impl := Impl; 
End;

{ PropertyDeclarationNode }
Constructor PropertyDeclarationNode.Create(ID: StringObject; T: TypeNode; Specs: PropertySpecifiersNode);
Begin
  Self.Name := ID;
  Self.DeclType := T;
  Self.Specifiers := Specs;
End;

{ ArrayPropertyNode }
Constructor ArrayPropertyNode.Create(Id:StringObject; T:TypeNode; Indexes:DeclarationListNode; Specs:PropertySpecifiersNode; Def:Boolean);
Begin
  Self.Name := ID;
  Self.DeclType := T;
  Self.Indexes := Indexes;
  Self.Specifiers := Specs;
  Self.IsDefault := Def;
End;

{ ObjectSectionNode }
Constructor ObjectSectionNode.Create(Fields, Decls:DeclarationListNode; Scope:ScopeEnum);
Begin
  Self.Fields := Fields;
  Self.Decls := Decls;
  //SCOPE?
End;

procedure ObjectSectionNode.Add(Sec:ObjectSectionNode);
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

procedure ObjectSectionNode.AddMethods(Methods:DeclarationListNode; Scope:ScopeEnum);
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
  Self.RefType := RefType;
End;

Constructor ClassRefTypeNode.Create(Qualifid: StringObject; Reftype:ClassTypeNode);
Begin
  Self.RefType :=RefType;
  Self.QualifID := Qualifid;
End;

{ MetaClassTypeNode }
Constructor MetaClassTypeNode.Create(baseType: TypeNode);
Begin
  Self.BaseType := BaseType;
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

Constructor StringLiteralNode.Create(Value: StringObject);
Begin
  Self.Value := Value;
End;

{ RealLiteralNode }

Constructor RealLiteralNode.Create(Value: Double);
Begin
  Self.Value := Value;
End;

{ PointerLiteralNode }
Constructor NullLiteralNode.Create();
Begin
End;

{ BinaryExpressionNode }
Constructor BinaryExpressionNode.Create(A, B: ExpressionNode);
Begin
  Self.Left := A;
  Self.Right := B;
End;

{ InExpressionNode }
Constructor InExpressionNode.Create(A, B: ExpressionNode);
Begin
  Self.Expr := A;
  Self._Set := B;
End;

{ SetRangeNode }
Constructor SetRangeNode.Create(_type: RangeTypeNode);
Begin
  RaiseError('TODO');
End;
{ TypeBinaryExpressionNode }
Constructor TypeBinaryExpressionNode.Create(Expr:ExpressionNode; ExprType: TypeNode);
Begin
  Self.Expr := Expr;
  Self.Types := ExprType;
End;

{ SimpleUnaryExpressionNode }
Constructor SimpleUnaryExpressionNode.Create(Expr: ExpressionNode);
Begin
  Self.Expr := Expr;
End;

{ SetExpressionNode }
Constructor SetExpressionNode.Create(Elements: ExpressionListNode);
Begin
  Self.Elements := Elements;
End;

{ LvalueAsExprNode }
Constructor LvalueAsExprNode.Create(lval: LvalueExpressionNode);
Begin
  Self.lval := LVal;
End;

{ ExprAsLvalueNode }
Constructor ExprAsLvalueNode.Create(Expr: ExpressionNode);
Begin
  Self.Expr := Expr;
End;

{ StaticCastNode }
Constructor StaticCastNode.Create(T:TypeNode; Expr:ExpressionNode);
Begin
  Self.CastType := T;
  Self.Expr := Expr;
End;

Constructor StaticCastNode.Create(T:TypeClassNode; Expr:ExpressionNode);
Begin
  Self.CastPrimitive := T;
  Self.Expr := Expr;
End;

{ ArrayAccessNode }
Constructor ArrayAccessNode.Create(_array:ArrayConstNode; Acessors:ExpressionListNode);
Begin
  Self._Array := _Array;
  Self.Acessors := Acessors;
End;

Constructor ArrayAccessNode.Create(lvalue:LvalueExpressionNode; Acessors:ExpressionListNode);
Begin
  Self.LValue := LValue;
  Self.Acessors := Acessors;
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
Constructor InheritedCallNode.Create(FuncName: StringObject; Args: ExpressionListNode);
Begin
  Self.FuncName := FuncName;
  Self.Args := Args;
End;

{ ObjectAccessNode }
Constructor ObjectAccessNode.Create(Obj: LvalueExpressionNode; Field:StringObject);
Begin
  Self.Obj := Obj;
  Self.Field := Field;
End;

{ IdentifierNode }
Constructor IdentifierNode.Create(Name: StringObject; T: TypeNode);
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
Constructor ConstDeclarationNode.Create(Name:StringObject; Init:ExpressionNode; T:TypeNode);
Begin
  Self.Name := Name;
  Self.Init := Init;
  Self.DeclType := T;
End;

{ EnumValueNode }
Constructor EnumValueNode.Create(Name:StringObject; Init:ExpressionNode);
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
Constructor RoutineDeclarationNode.Create(Name: StringObject; Params: ParametersSectionNode; Ret: TypeNode; Dirs: FunctionDirectiveListNode);
Begin
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := Ret;
  Self.Directives := Dirs;
End;

{ MethodDeclarationNode }
Constructor MethodDeclarationNode.Create(objname, name:StringObject; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Kind:MethodKindEnum);
Begin
  Self.Objname := ObjName;
  Self.Name := Name;
  Self.DeclaringSection := Params;
  Self.ResultType := Ret;
  Self.Directives := Dirs;
  Self.Kind := Kind;
End;

{ RoutineDefinitionNode }
Constructor RoutineDefinitionNode.Create(Name:StringObject; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Body: RoutineSectionNode);
Begin

End;

Constructor RoutineDefinitionNode.Create(Name:StringObject; SignatureType:ProceduralTypeNode; Dirs:FunctionDirectiveListNode; Body:RoutineSectionNode);
Begin
  Self.Name := Name;
  Self.SignatureType := SignatureType;
  Self.Directives := Dirs;
  Self.Body := Body;
End;

{ MethodDefinitionNode }
Constructor MethodDefinitionNode.Create(objname, name: StringObject; Params:ParametersSectionNode; Ret:TypeNode; Dirs:FunctionDirectiveListNode; Kind: MethodKindEnum; Body:RoutineSectionNode);
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
Constructor CompositeDeclarationNode.Create(Name: StringObject; Ctype:CompositeTypeNode);
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
Constructor LabelStatementNode.Create(Name: StringObject; Statement:StatementNode);
Begin
  Self.Name := Name;
  Self.Statement := Statement;
End;

{ GotoStatementNode }
Constructor GotoStatementNode.Create(LabelName: StringObject);
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

Constructor OnStatementNode.Create(Ident, _type: StringObject; Body:StatementNode);
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
Constructor VariantDeclarationNode.Create(ID: StringObject; T: TypeNode; Fields: DeclarationListNode);
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
Constructor ExportItemNode.Create(name: StringObject; pars: ParametersSectionNode; exportname: StringObject);
Begin
  Self.Name := Name;
  Self.FormalParams := Pars;
  Self.ExportName := ExportName;
End;

Constructor ExportItemNode.Create(name: StringObject; pars: ParametersSectionNode; Index: Integer);
Begin
  Self.Name := Name;
  Self.FormalParams := Pars;
  Self.Index := Index;
End;

{ UnresolvedNode }
Constructor UnresolvedNode.Create(ID: StringObject);
Begin
  Self.ID := ID;
End;

{ TypeListNode }
Function TypeListNode.Get(Index: Integer): TypeNode;
Begin
  Result := TypeNode(Self.Get(Index));
End;

{ DeclarationListNode }
Function DeclarationListNode.Get(Index: Integer): DeclarationNode;
Begin
  Result := DeclarationNode(Self.Get(Index));
End;

{ UnitListNode }
Function UnitListNode.Get(Index: Integer): UnitItemNode;
Begin
  Result := UnitItemNode(Self.Get(Index));
End;

{ StatementListNode }
Constructor StatementListNode.Create(St: StatementNode);
Begin
  Self.List := ListNode.Create(St);
End;

Procedure StatementListNode.Add(St: StatementNode);
Begin
  Self.List.Add(St);
End;

Function StatementListNode.Get(Index: Integer): StatementNode;
Begin
  Result := StatementNode(List.Get(Index));
End;


{ ExpressionListNode }
Function ExpressionListNode.Get(Index: Integer): ExpressionNode;
Begin
  Result := ExpressionNode(Self.Get(Index));
End;

{ EnumValueListNode }
Function EnumValueListNode.Get(Index: Integer): EnumValueNode;
Begin
  Result := EnumValueNode(Self.Get(Index));
End;

{ IdentifierListNode }

Function IdentifierListNode.Get(Index: Integer): IdentifierNode;
Begin
  Result := IdentifierNode(Self.Get(Index));
End;

End.
