Program xpc;

{$I terra.inc}

Uses LexLib, YaccLib, TERRA_Utils, TERRA_IO, TERRA_Error, TERRA_FileIO, TERRA_Classes;

//    | LBRAC lvalue RBRAC                { $$ := $2; }

{Var
  yylval:YYSType;}

Const
  MaxStates = 128;

Type
  PascalLexer = Class(TCustomLexer)
    Protected
      _Source:Stream;

      _bufptr:Integer;
      _buf:Array[1..max_chars] Of AnsiChar;

      _StateIndex:Integer;
      _StateBuffer:Array[1..MaxStates] Of Integer;
      _CurrentState:Integer;

      // obtain one character from the input file (null character at end-of-file)
      function get_char: AnsiChar; Override;
      // return one character to the input file to be reread in subsequent calls to get_char
      procedure unget_char(c: AnsiChar); Override;

      Procedure yypushback(Count:Integer);
      Function yylength:Integer;

      procedure yyclear; override;
      function yywrap: boolean; override;

      Procedure yybegin(newState:Integer);
      Procedure pushstate(State:Integer);
      Procedure popstate();

      Function yylaststate:Integer;

      Procedure switchstate(State:Integer);
      Procedure switchCallback(Callback, GotoState:Integer);

      Procedure yyerror(Const S:AnsiString);

    Public
      Constructor Create(Source:Stream);
      Destructor Destroy; Override;

      Function Parse: integer; Override;
  End;

  PascalParser = Class(TCustomParser)
    Protected
      _Lexer:PascalLexer;
    Public

      Function Parse: integer; Override;

      Property Lexer:PascalLexer Read _Lexer;
  End;


Const
  functionDirective_Assembler = 1;
  functionDirective_Export    = 2;
  functionDirective_Inline    = 3;
  functionDirective_VarArgs   = 4;
  functionDirective_Far       = 5;
  functionDirective_Near      = 6;
  functionDirective_Resident  = 7;
  functionDirective_Overload  = 8;

  methodDirective_Abstract  = 9;
  methodDirective_Override = 10;
  methodDirective_Virtual = 11;
  methodDirective_Dynamic = 12;
  methodDirective_Reintroduce = 13;

  callConvention_Pascal = 0;
  callConvention_SafeCall = 1;
  callConvention_StdCall = 2;
  callConvention_CDecl = 3;
  callConvention_Register = 4;

	importDirective_Default = 0;
  importDirective_External = 1;
  importDirective_Forward = 2;

  scope_Public    = 0;
  scope_Protected = 1;
  scope_Private   = 2;
  scope_Published = 3;

Type
  PascalNode = Class(ListObject)
  End;

	SourceNode = Class(PascalNode)
    Name:StringObject;
  End;

  ProgramSection = Class;
  NodeList = List;

  TypeNode = Class(PascalNode)
		// TODO each derive should set the typesize
		typeSize:Integer;
  End;

  MetaTypeClass = Class Of TypeNode;

  MetaType = Class(TypeNode)
    Value:MetaTypeClass;

    Constructor Create(TypeClass:MetaTypeClass);
  End;

  Declaration = Class(PascalNode)
		Name:StringObject;
		DeclType:TypeNode;

		Constructor Create(Name:StringObject; T:TypeNode = Nil);
  End;

  DeclarationList = Class(PascalNode)
    Declarations:Array Of Declaration;
    Count:Integer;

    Constructor Create(Decl:Declaration = Nil);
    Procedure Add(Decl:Declaration); Overload;
    Procedure Add(DeclList:DeclarationList); Overload;

    Procedure InsertAt(Index:Integer; DeclList:DeclarationList);
  End;

  UnitItem = Class(Declaration)
    Location:StringObject;

    Constructor Create(Name:StringObject; Location:StringObject); Overload;
    Constructor Create(Name:StringObject); Overload;
  End;

	Statement = Class(PascalNode)
  End;

  BlockStatement = Class;

	ProgramNode = Class(SourceNode)
    Section:ProgramSection;

    Constructor Create(Name:StringObject; UsesList:NodeList; Decls:DeclarationList; Body:BlockStatement);
  End;

  ObjectSection = Class;
  CompositeDeclaration = Class;
  CompositeType = Class(TypeNode)
		//heritage:StringList;

		section:ObjectSection ;

		decl:CompositeDeclaration;

		IsPacked:Boolean;

		// optional
		Name:StringObject;


    Ancestors:Array Of CompositeType;
    AncestorCount:Integer;

		//public List<CompositeType> ancestors;

		Function IsForward:Boolean;  
  End;

	Section = Class(PascalNode)
		Decls:DeclarationList;

		Constructor Create(Decls:DeclarationList);
  End;

  TopLevelDeclarationSection = Class(Section)
    Useslist:NodeList;

		Constructor Create(UsesList:NodeList; Decls:DeclarationList);
  End;


	InterfaceSection = Class(TopLevelDeclarationSection)
  End;

	ImplementationSection = Class(TopLevelDeclarationSection)
  End;

  StatementList = Class(Statement)
    Statements:Array Of Statement;
    Count:Integer;

    Constructor Create(St:Statement);
    Procedure Add(St:Statement);
  End;

	BlockStatement = Class(Statement)
		List:StatementList;

		Constructor Create(List:StatementList);
  End;

	ProgramSection = Class(TopLevelDeclarationSection)
    Block:BlockStatement;

    Constructor Create(UsesList:NodeList; Decls:DeclarationList; Code:BlockStatement);
  End;

	ConstantValue = Class(PascalNode)
  End;

  Expression = Class(PascalNode)
		ForcedType:TypeNode;
    EnforceConst:Boolean;
  End;

  ExpressionList = Class(PascalNode)
    Expressions:Array Of Expression;
    Count:Integer;

    Constructor Create(Exp:Expression = Nil);
    Procedure Add(Exp:Expression);

    Procedure InsertAt(Index:Integer; Exp:Expression);
  End;

  Literal = Class;
  IntLiteral = Class;
  
  ConstExpression = Class(Expression)
		Value:ConstantValue;

    Function ResolveToLiteral():Literal;
  End;

	LabelDeclaration = Class(Declaration)
  End;

	ValueDeclaration = Class(Declaration)
  End;

	VarDeclaration = Class(ValueDeclaration)
		Init:Expression;
		AbsoluteID:StringObject;
		IsThrVar:Boolean;

		Constructor Create(Name:StringObject; VarType:TypeNode; Init:Expression; AbsoluteId:StringObject);  Overload;
		Constructor Create(Name:StringObject; VarType:TypeNode; Init:Expression = Nil);  Overload;
  End;

  ParameterKind = (param_Default, param_Var, param_Out, param_Const);

	/// Routine parameters. May be value (default), variable, constant, or out.
	/// Param types must be an id, string or open array (array of paramtype)
	ParamDeclaration = Class(ValueDeclaration)
		Init:Expression;
    Kind:ParameterKind;

		Constructor Create(Name:StringObject; ParamType:TypeNode; Init:Expression; Kind:ParameterKind);
  End;

	VarParamDeclaration = Class(ParamDeclaration)
		Constructor Create(Name:StringObject; VarType:TypeNode);
  End;

	ConstParamDeclaration = Class(ParamDeclaration)
		Constructor Create(Name:StringObject; ConstType:TypeNode; Init:Expression = Nil);
  End;

	OutParamDeclaration = Class(ParamDeclaration)
		Constructor Create(Name:StringObject; VarType:TypeNode);
  End;


  ParametersSection = Class(Section)
		  ReturnVar:ParamDeclaration; //TODO

		  Constructor Create(Decls:DeclarationList=Nil);
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
  FunctionDirective = Integer;

	FunctionDirectiveList  = Class(PascalNode)
		CallConv:Integer;
		Directives:Array Of FunctionDirective;

    Constructor Create(Dirs:FunctionDirective);
		Procedure Add(dirs:FunctionDirective);
		//public virtual void Add(int dir)
		Function Contains(dir:FunctionDirective ):Boolean;
		/// Checks the immediate coherence between function directives.
		/// Must be called after all directives are added
		Function CheckDirectives():Boolean;
  End;

  ProceduralType = Class;

	/// Declaration of a Callable unit, i.e. a global routine or method
	CallableDeclaration = Class(Declaration)
		/// Gets the fully qualified name of this callable
		/// (obj+metname for methods, plus parameter types for overloads)
		/// To be set by the Resolver
		QualifiedName:String ;

		/// Section that declares this callable.
		/// To be set by resolver
		declaringSection:Section ;

    SignatureType:ProceduralType;
    ResultType:TypeNode;

    Directives:FunctionDirectiveList;

		Constructor Create(name:string ; params:ParametersSection; ret:TypeNode=Nil; dirs:FunctionDirectiveList =Nil);
  End;


	RoutineSection = Class(Section)
    block:Statement;

		 // to be set by resolver
		declaringCallable:CallableDeclaration;

		Constructor Create(decls:DeclarationList ; block:Statement );
  End;

	LibraryNode = Class(SourceNode)
		section:ProgramSection;

		Constructor Create(Name:StringObject; usesList:NodeList; decls:DeclarationList; body:BlockStatement);
  End;

	UnitNode = Class(SourceNode)
		interfaces:InterfaceSection;
		Implements:ImplementationSection;
		Inits:BlockStatement;
		Final:BlockStatement;

		Constructor Create(Name:StringObject; interfce:InterfaceSection; impl:ImplementationSection;  init:BlockStatement=Nil; final:BlockStatement=Nil);
  End;

  PackageNode = Class(SourceNode)
		requires:NodeList;
		contains:NodeList;

		Constructor Create(Name:StringObject; requires, contains:NodeList);
  End;

  StructuredConstant = Class(ConstExpression)
    Exprlist:ExpressionList;

		Constructor Create(exprlist:ExpressionList);
  End;

	ArrayConst = Class(StructuredConstant)
    Constructor Create(exprlist:ExpressionList); Overload;
		Constructor Create(arrayElems:StringObject); Overload;
			//: base(new ExpressionList(arrayElems.ToCharArray().Select(x => new CharLiteral(x)))){ }
	End;

  FieldInit = Class(ConstExpression)
	  fieldname:StringObject;
    expr:Expression;

    Constructor Create(name:StringObject; expr:Expression);
  End;

  FieldInitList = Class(PascalNode)
    Fields:Array Of FieldInit;
    Count:Integer;

    Constructor Create(F:FieldInit = Nil);
    Procedure Add(F:FieldInit);
  End;

  RecordConst = Class(StructuredConstant)
    Constructor Create(exprlist:FieldInitList);
  End;

  ConstIdentifier = Class(ConstExpression)
    name:AnsiString;

    Constructor Create(Name:StringObject);
  End;

  ScalarType = Class(TypeNode)
  End;

  TypeClass = Class Of ScalarType;

  IntegralType = Class(ScalarType)
		// Should not be used
    Function GetMinValue():Int64 ; Virtual;
    Function GetMaxValue():Int64 ; Virtual;
  End;

	IntegerType = Class(IntegralType)
  End;

	SignedIntegerType = Class(IntegerType)
  End;

	UnsignedIntegerType = Class(IntegerType)
  End;

	UnsignedInt8Type = Class(UnsignedIntegerType)		// byte
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
	End;

	BoolType = Class(IntegralType)
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	CharType = Class(IntegralType)
    Function GetMinValue():Int64 ; Override;
    Function GetMaxValue():Int64 ; Override;
	End;

	RealType = Class(ScalarType)
  End;

	FloatType = Class(RealType)
  End;

	DoubleType = Class(RealType)
  End;

	ExtendedType = Class(RealType)
  End;

	CurrencyType = Class(RealType)
  End;

	StringType = Class(ScalarType)
    Length:Expression;
    
    Constructor Create(Len:Expression);
	End;

	FixedStringType = Class(StringType)
		expr:Expression ;
		Len:Integer;

		Constructor Create(expr:Expression);
  End;

	RangeType = Class(TypeNode)
		min:Expression ;
		max:Expression ;

		Constructor Create(min, max:Expression );
  End;

  EnumValue = Class;
  EnumValueList = Class(PascalNode)
		Values:Array Of EnumValue;
    Count:Integer;

    Constructor Create(Val:EnumValue);
    Procedure Add(Val:EnumValue);
  End;

	EnumType = Class(TypeNode)
    List:EnumValueList;

		Constructor Create(enumVals:EnumValueList);
		{
			this.enumVals = enumVals;
		}
  End;

	/// Variants can hold values of any type except records, sets, static arrays, files, classes, class references, and pointers.
	/// I.e. can hold anything but structured types and pointers.
	/// They can hold interfaces, dynamic arrays, variant arrays
	VariantType = Class(TypeNode)
		actualtype:TypeNode ;
  End;

	/// PointedType may be any type.
	/// It may be a not yet declared type (forward declaration)
	PointerType = Class(ScalarType)
		pointedType:TypeNode ;

		Constructor Create(pointedType:TypeNode );
  End;


	FieldDeclaration = Class(ValueDeclaration)
		isStatic:Boolean;
		scope:Integer;
		declaringObject:CompositeType ;

		Constructor FieldDeclaration(id:String ; t:TypeNode = Nil; isStatic:boolean  =false);
  End;

	PropertySpecifiers  = Class(PascalNode)
		Public
      index:IntLiteral;
		  read:AnsiString;
		  write:AnsiString;
      stored:ConstExpression;
		  default:Literal;	// nodefault == Int32.MaxValue
		  impl:AnsiString;

		  Constructor Create(read, write:StringObject); Overload;
		  {Begin
			  this.read = read;
			  this.write = write;
		  End;}

      Constructor Create(index:IntLiteral; read, write:StringObject; stored:ConstExpression; default:Literal); Overload;
		{
			this.index = index;
			this.stored = stored;
			this.@default = @default;
		}

		  Constructor Create(index:IntLiteral; read, write:StringObject; stored:ConstExpression; default:Literal; impl:StringObject); Overload;
		{
			this.impl = impl;
		}
  End;

	PropertyDeclaration = Class(FieldDeclaration)
    Public
  		specifiers:PropertySpecifiers;

		  IsStatic:Boolean;

		  Constructor Create(ident:StringObject; Ttype:TypeNode ; specs:PropertySpecifiers  = Nil);
		{
			this.specifiers = specs;

			if (type != null) // no override
				if (specs.read == null && specs.write == null)
					Error("Class property must have at least a Read of Write specified");
		}
  End;

	ArrayProperty = Class(PropertyDeclaration)
		indexes:DeclarationList;
		isDefault:Boolean;

		Constructor Create(ident:StringObject; ttype:TypeNode; indexes:DeclarationList; specs:PropertySpecifiers; def:Boolean);
		{
			this.indexes = indexes;
			this.specifiers = specs;
			this.isDefault = def;
		}
	End;


  MethodDeclaration = Class;

	ObjectSection = Class(Section)
		fields:DeclarationList ;
		properties:DeclarationList ;
		declaringObject:CompositeType ;

		// Kept in the baseclass, 'decls' list
	//	public DeclarationList methods;

		Constructor Create(fs:DeclarationList = Nil; ds:DeclarationList  = Nil; scope:Integer = Scope_Published);
 		{
			fields = fs;
			if (fields == null)
				fields = new DeclarationList();
			properties = new DeclarationList();

			AddDecls(ds, s);	// methods and properties

			if (Enum.IsDefined(typeof(Scope), s))
				foreach (FieldDeclaration d in fields)
					d.SetScope(s);
		}

		Procedure Add(sec:ObjectSection );
		{
			if (sec == null)
				return;

			fields.Add(sec.fields);
			decls.Add(sec.decls);
			properties.Add(sec.properties);
		}

		Procedure AddFields(fs:DeclarationList; Scope:Integer);
		{
			if (fs != null)
				foreach (FieldDeclaration d in fs)
					d.SetScope(s);
			fields.Add(fs);
		}

		Procedure AddMethods(fs:DeclarationList ; Scope:Integer);
		{
			if (fs != null)
				foreach (MethodDeclaration d in fs)
					d.SetScope(s);
			decls.Add(fs);
		}

		Procedure AddProperties(fs:DeclarationList ; Scope:Integer);
		{
			if (fs != null)
				foreach (PropertyDeclaration d in fs)
					d.SetScope(s);
			properties.Add(fs);
		}

		/// Add unknown-type declarations
		Procedure AddDecls(fs:DeclarationList; Scope:Integer);
		{
			if (fs == null)
				return;

			if (s != 0)
				foreach (IScopedDeclaration d in fs)
					d.SetScope(s);

			foreach (var d in fs)
			{
				if (d is MethodDeclaration)
					decls.Add(d);
				else if (d is PropertyDeclaration)	// a property is a field too
					properties.Add(d);
				else if (d is FieldDeclaration)	// a property is a field too
					fields.Add(d);
				else
					ErrorInternal("Unknown Declaration in ObjectSection AddDecls");
			}

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
		Function GetMember(id:String ):Declaration ;
		{
			Declaration d;
			if ((d = fields.GetDeclaration(id)) != null)
				return d;
			if ((d = decls.GetDeclaration(id)) != null)
				return d;
			if ((d = properties.GetDeclaration(id)) != null)
				return d;
			return null;
		}

		/// Returns a method with the given name
		Function GetMethod(id:String ):MethodDeclaration;
		{
			return decls.GetDeclaration(id) as MethodDeclaration;
		}

		/// <summary>
		/// Returns a field with the given name
		/// </summary>
		Function GetField(id:String ):FieldDeclaration ;
		{
			return fields.GetDeclaration(id) as FieldDeclaration;
		}

		/// <summary>
		/// Returns a property with the given name
		/// </summary>
		Function GetProperty(id:String ):PropertyDeclaration;
		{
			return properties.GetDeclaration(id) as PropertyDeclaration;
		}

  End;

  ClassType = Class(CompositeType)
		_self:FieldDeclaration ;

    Public
		  Constructor Create(heritage:NodeList; sec:ObjectSection  = Nil);
		{
			if (!IsForward)
			{
				self = new FieldDeclaration("self", new ClassRef(this.Name, this));
				self.SetScope(Scope.Protected);
				section.fields.Add(self);
			}
  End;

	ClassRefType = Class(ClassType)
		qualifid:StringObject;
		reftype:ClassType;

		Constructor Create(reftype:ClassType); Overload;
		{
			: base(new ArrayList())
			this.qualifid = reftype.Name;
			this.reftype = reftype;
		}

		Constructor Create(qualifid:StringObject; reftype:ClassType = Nil); Overload;
		{
			: base(new ArrayList())
			this.qualifid = qualifid;
			this.reftype = reftype;
		}


  End;

	MetaclassType = Class(ScalarType)
		baseType:TypeNode ;

		Constructor Create(baseType:TypeNode );
  End;

  Literal = Class(ConstExpression)
    Constructor Create(val:ConstantValue; t:TypeNode);
  End;

	OrdinalLiteral = Class(Literal)
    Constructor Create(v:Cardinal; t:IntegralType);
  End;

	IntLiteral = Class(OrdinalLiteral)
    Constructor Create(Value:Int64);
  End;

	CharLiteral = Class(OrdinalLiteral)
    Constructor Create(Value:AnsiChar);
  End;

	BoolLiteral = Class(OrdinalLiteral)
    Constructor Create(Value:Boolean);
  End;

	StringLiteral = Class(Literal)
    Constructor Create(Value:StringObject);
  End;

	RealLiteral = Class(Literal)
    Constructor Create(Value:Double);
  End;

	PointerLiteral = Class(Literal)
    Constructor Create(val:Int64);
  End;

	// For ints, chars and bools
	IntegralValue = Class(ConstantValue)
    Val:Cardinal;

    Constructor Create(val:Int64);
  End;

	StringValue = Class(ConstantValue)
		val:AnsiString;
		Constructor Create(val:String);
  End;

	RealValue = Class(ConstantValue)
		val:double;
		Constructor RealValue(val:double );
  End;

	BinaryExpression = Class(Expression)
		left:Expression;
		right:Expression;

    Constructor Create(e1, e2:Expression );
  End;

	SetIn = Class(BinaryExpression)
		expr:Expression;
		_set:Expression ;		// enforce that 'set' is in fact a set

		Constructor Create(e1,e2:Expression );
  End;

	SetRange = Class(BinaryExpression)
		Constructor Create(_type:RangeType);
		{
			this.ForcedType = this.Type = type;
			this.EnforceConst = true;
		}
  End;

  ArithmeticBinaryOp = (op_ADD, op_SUB, op_DIV, op_MUL, op_QUOT, op_MOD, op_SHR, op_SHL);

	ArithmeticBinaryExpression  = Class(BinaryExpression)
    Op:ArithmeticBinaryOp;
  End;

	Subtraction = Class(ArithmeticBinaryExpression)
  End;

	Addition = Class(ArithmeticBinaryExpression)
	End;

	Product = Class(ArithmeticBinaryExpression)
  End;

	Division = Class(ArithmeticBinaryExpression)
  End;

	// Integer division
	Quotient = Class(ArithmeticBinaryExpression)
  End;

	Modulus = Class(ArithmeticBinaryExpression)
  End;

	ShiftRight = Class(ArithmeticBinaryExpression)
  End;

	ShiftLeft = Class(ArithmeticBinaryExpression)
  End;


	LogicalBinaryOp = (op_AND, op_OR, op_XOR);

	LogicalBinaryExpression = Class(BinaryExpression)
		  op:LogicalBinaryOp ;
  End;

	ComparisonBinaryOp = (op_EQ, op_NE, op_LT, op_LE, op_GT, op_GE, op_SGT, op_SGE,op_SLT,op_SLE);

	ComparisonBinaryExpression = Class(BinaryExpression)
		op:ComparisonBinaryOp;
  End;

  LogicalAnd = Class(LogicalBinaryExpression)
  End;

	LogicalOr = Class(LogicalBinaryExpression)
  End;


	LogicalXor = Class(LogicalBinaryExpression)
  End;


	Equal = Class(ComparisonBinaryExpression)
  End;


	NotEqual = Class(ComparisonBinaryExpression)
  End;


	LessThan = Class(ComparisonBinaryExpression)
  End;


	LessOrEqual = Class(ComparisonBinaryExpression)
  End;


	GreaterThan = Class(ComparisonBinaryExpression)
  End;


	GreaterOrEqual = Class(ComparisonBinaryExpression)
  End;


	TypeBinaryExpression = Class(BinaryExpression)
		expr:Expression ;
		types:TypeNode ;

		Constructor Create(e1:Expression ; e2:TypeNode );
  End;

	TypeIs = Class(TypeBinaryExpression)
  End;

	RuntimeCast = Class(TypeBinaryExpression)
  End;

	UnaryExpression = Class(Expression)
  End;

	SimpleUnaryExpression = Class(Expression)
		expr:Expression ;

		Constructor Create(expr:Expression );
  End;

	UnaryPlus = Class(SimpleUnaryExpression)
  End;

	UnaryMinus = Class(SimpleUnaryExpression)
  End;

	LogicalNot = Class(SimpleUnaryExpression)
  End;

	AddressLvalue = Class(SimpleUnaryExpression)
  End;


	SetExpression = Class(UnaryExpression)
		setelems:ExpressionList ;
		Constructor Create(elems:ExpressionList = Nil);
  End;

  LvalueExpression = Class;

	/// Cast an lvalue to an rvalue (Expr)
	LvalueAsExpr = Class(UnaryExpression)
		lval:LvalueExpression;

		Constructor Create(lval:LvalueExpression);
  End;

	LvalueExpression = Class(UnaryExpression)
  End;

	/// Cast an rvalue (Expr) to an lvalue
	ExprAsLvalue = Class(LvalueExpression)
		expr:Expression ;

		Constructor Create(expr:Expression );
  End;

	/// VarType(expr)
	StaticCast = Class(LvalueExpression)
		casttype:TypeNode;
    castprimitive:TypeClass;
		expr:Expression ;

		Constructor Create(t:TypeNode; e:Expression ); Overload;
    Constructor Create(t:TypeClass; e:Expression); Overload;
  End;

	ArrayAccess = Class(LvalueExpression)
		lvalue:LvalueExpression ;
		acessors:ExpressionList ;
		// object alternative to lvalue
		_array:ArrayConst;

		Constructor Create(_array:ArrayConst; acessors:ExpressionList ); Overload;
		Constructor Create(lvalue:LvalueExpression; acessors:ExpressionList ); Overload;
  End;

	PointerDereference = Class(LvalueExpression)
		expr:Expression ;

		Constructor Create(expr:Expression );
  End;

	RoutineCall = Class(LvalueExpression)
		func:LvalueExpression ;
		args:ExpressionList ;

		Constructor Create(func:LvalueExpression ; retType:TypeNode  = Nil);
	//RoutineCall(LvalueExpression func, ExpressionList args, TypeNode retType = null)
  End;

	InheritedCall = Class(RoutineCall)
		funcname:StringObject;
		// to be set by resolver
		declaringObject:CompositeType ;

		Constructor Create(funcname:StringObject; args:ExpressionList =Nil);
  End;

	/// An access to a member in an object (record, class or interface)
	ObjectAccess = Class(LvalueExpression)
		obj:LvalueExpression ;
		field:StringObject;

		Constructor Create(obj:LvalueExpression; field:StringObject);
  End;

	/// Identifier that refers to a named declaration
	Identifier = Class(LvalueExpression)
    name:StringObject;
		decl:Declaration ;

		Constructor Create(val:StringObject; t:TypeNode  = Nil);
  End;

	/// Identifier that refers to a named class. Static access
	IdentifierStatic = Class(Identifier)
  End;


	UnresolvedLvalue = Class(LvalueExpression)
	End;

	UnresolvedId = Class(UnresolvedLvalue)
		id:Identifier ;

		Constructor Create(val:Identifier );
  End;

	/// Call, to be resolver after parsing
	UnresolvedCall = Class(UnresolvedLvalue)
		func:LvalueExpression ;
		args:ExpressionList ;

		Constructor Create(lval:LvalueExpression ; args:ExpressionList  = Nil);
  End;


	/// TODO!! Must Derive type
	ConstDeclaration = Class(ValueDeclaration)
		init:Expression ;

		Constructor Create(Name:StringObject; init:Expression; t:TypeNode  = Nil);
  End;

	EnumValue = Class(ConstDeclaration)
    Constructor Create(Name:StringObject; init:Expression = Nil);
  End;

	/// Creates a custom, user-defined name for some Type
	TypeDeclaration  = Class(Declaration)
  End;

	ProceduralType  = Class(TypeNode)
		params:ParametersSection;

		/// Function's return type. Must be null for every non-function routine.
		funcret:TypeNode ;

		Directives:FunctionDirectiveList ;

		Constructor Create(params:ParametersSection ; ret:TypeNode  = Nil; dirs:FunctionDirectiveList  = Nil);
  End;

	MethodKind = (method_Default,	method_Constructor, method_Destructor);

	MethodType = Class(ProceduralType)
		kind:MethodKind ;
  End;

	/// Declaration of a global Routine
	RoutineDeclaration = Class(CallableDeclaration)
		Constructor Create(name:StringObject; params:ParametersSection;  ret:TypeNode  = Nil; dirs:FunctionDirectiveList  = Nil);
  End;


	/// Declaration of a Method
	MethodDeclaration = Class(CallableDeclaration)
		isStatic:Boolean;
		objname:AnsiString;
		fullname:AnsiString;
    Scope:Integer;
		declaringObject:CompositeType ;

		Constructor Create(objname:StringObject; name:StringObject; params:ParametersSection; ret:TypeNode = Nil; dirs:FunctionDirectiveList  = Nil; kind:MethodKind  = method_Default);
		{
			this.objname = objname;
			isStatic = false;
			fullname = objname + "." + name;

			if (Directives == null)
				Directives = new MethodDirectives();

			foreach (var param in @params.decls)
				if (param.name == "self")
					throw new IdentifierRedeclared("Method parameter cannot shadow 'self' reference");
		}
	End;

	/// Routine definition (implementation)
	RoutineDefinition = Class(RoutineDeclaration)
		body:RoutineSection ;

		Constructor Create(name:StringObject; params:ParametersSection;  ret:TypeNode  = Nil; dirs:FunctionDirectiveList  = nil; body:RoutineSection  = Nil); Overload;
    Constructor Create(name:StringObject; signatureType:ProceduralType; dirs:FunctionDirectiveList  = nil; body:RoutineSection  = Nil); Overload;
  End;

	/// Method definition (implementation)
	MethodDefinition = Class(MethodDeclaration)
		body:RoutineSection ;

		Constructor Create(objname:StringObject; name:StringObject; params:ParametersSection; ret:TypeNode  = Nil; dirs:FunctionDirectiveList = Nil; kind:MethodKind  = Method_Default; body:RoutineSection  = Nil);
  End;

	CompositeDeclaration = Class(TypeDeclaration)

		Constructor Create(name:StringObject; ctype:CompositeType);
  End;

	ClassDeclaration = Class(CompositeDeclaration)
		Constructor Create(name:StringObject; ctype:ClassType);
  End;

  InterfaceType = Class;
	InterfaceDeclaration = Class(CompositeDeclaration)
		Constructor Create(name:StringObject; ctype:InterfaceType);
  End;

	/// Routine Directives

 {	MethodDirectives = Class(FunctionDirectiveList)
		methoddirs:Array Of FunctionDirective;

		Procedure Add(dir:FunctionDirective);
		Function Contains(dir:FunctionDirective):Boolean;
		Function CheckDirectives():Boolean;
  End;}

	ExternalDirective = Class(PascalNode)
		_File:Expression;
		Name:Expression;

		Constructor Create(_file:Expression; name:Expression  = Nil);
  End;

	ImportDirectives = Class(FunctionDirectiveList)
		Importdir:FunctionDirective;
		_external:ExternalDirective;

    Constructor Create(ImportDir:FunctionDirective);
		Procedure Add(dirs:FunctionDirectiveList );
		Function Contains(dir:FunctionDirectiveList):Boolean;
  End;


	LabelStatement = Class(Statement)
		_label:StringObject;
		stmt:Statement ;

		// to be set by the resolver
		decl:LabelDeclaration ;

		Constructor Create(_label:StringObject; stmt:Statement);
  End;

	GotoStatement = Class(Statement)
		gotolabel:StringObject;

		// to be set by the resolver
		decl:LabelDeclaration ;

		Constructor Create(_label:StringObject);
  End;

	EmptyStatement = Class(Statement)
  End;

	BreakStatement = Class(Statement)
  End;

	ContinueStatement = Class(Statement)
  End;

	Assignment = Class(Statement)
		lvalue:LvalueExpression ;
		expr:Expression ;

		Constructor Create(lvalue:LvalueExpression ; expr:Expression );
  End;

	IfStatement = Class(Statement)
		condition:Expression ;
		thenblock:Statement ;
		elseblock:Statement ;

		Constructor Create(condition:Expression ; ifTrue:Statement ;  ifFalse:Statement=Nil);
  End;

	ExpressionStatement = Class(Statement)
		expr:Expression ;

		Constructor Create(expr:Expression);
  End;

	CaseSelector = Class(Statement)
		list:ExpressionList ;
		stmt:Statement ;

		Constructor Create(list:ExpressionList ;stmt:Statement );
  End;

	CaseStatement = Class(Statement)
		condition:Expression ;
		selectors:StatementList ;
		caseelse:Statement ;

		Constructor Create(condition:Expression ; selectors:StatementList ; caseelse:Statement );
  End;

	LoopStatement = Class(Statement)
		condition:Expression ;
		block:Statement ;

		Constructor Create(block:Statement ; condition:Expression );
  End;

	RepeatLoop = Class(LoopStatement)
  End;

	WhileLoop = Class(LoopStatement)
  End;

	ForLoop = Class(LoopStatement)
		_var:Identifier ;
		start:Expression ;
		_end:Expression;
		direction:Integer;

		Constructor Create(_var:Identifier ; start:Expression ; _end:Expression ; body:Statement ; dir:Integer);
  End;

	WithStatement = Class(Statement)
		_with:ExpressionList;
		body:Statement ;

		Constructor Create(_with:ExpressionList; body:Statement );
  End;

	TryFinallyStatement = Class(Statement)
		body:BlockStatement ;
		final:BlockStatement ;

		Constructor Create(body, final:BlockStatement );
  End;

	ExceptionBlock = Class(Statement)
  	onList:StatementList ;
		default:BlockStatement;	// else or default, same semantics

		Constructor Create(onList:StatementList; default:BlockStatement  = Nil);
  End;

	TryExceptStatement = Class(Statement)
		body:BlockStatement ;
		final:BlockStatement ;

		Constructor Create(body:BlockStatement; final:ExceptionBlock);
  End;


	RaiseStatement = Class(Statement)
		lvalue:LvalueExpression ;
		expr:Expression ;

		Constructor Create(lvalue:LvalueExpression ; expr:Expression );
  End;

	OnStatement = Class(Statement)
		ident:StringObject;
		_type:StringObject;
		body:Statement ;

		Constructor Create(ident, _type:StringObject; body:Statement);
  End;

	AssemblerBlock = Class(BlockStatement)
		Constructor AssemblerBlock(asmInstrs:StatementList );
  End;

  InterfaceList = List;

	InterfaceType = Class(CompositeType)
		guid:StringLiteral;

		Constructor Create(heritage:InterfaceList; ssec:ObjectSection = Nil; guid:StringLiteral  = Nil);
  End;


	///			StructuredType > Type
	///				Array > Type
	///				Set	  > OrdinalType 
	///				File  > VariableType
	///		 		Record > TypeNode ...

	StructuredType = Class(TypeNode)
		basetype:TypeNode;
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

	RecordType = Class(StructuredType)
		compTypes:DeclarationList;

		Constructor Create(compTypes:DeclarationList);
  End;

	RecordFieldDeclaration = Class(ValueDeclaration)
		Constructor Create(id:StringObject; t:TypeNode  = Nil);
  End;

  ArrayType = Class(StructuredType)
		dimensions:Array Of Integer;
    dimensionCount:Integer;

		Constructor Create(baseType:TypeNode; dims:List); Overload;
		Constructor Create(baseType, sizeType:TypeNode); Overload;
    Constructor Create(sizeType:TypeNode); Overload;

		Procedure AddDimension(size:Integer);
  End;

  SetType = Class(StructuredType)
    Constructor Create(T:TypeNode);
  End;

  FileType = Class(StructuredType)
    Constructor Create(T:TypeNode);
  End;

	VariantDeclaration = Class(RecordFieldDeclaration)
		varfields:DeclarationList ;

		Constructor Create(id:StringObject; t:TypeNode ; varfields:DeclarationList );
		{
			// TODO
			///	if (!(t is IOrdinalType))
			//		throw new TypeRequiredException("Ordinal");
			this.varfields = varfields;
		}

  End;

	/// Variant case entry declaration
	VarEntryDeclaration = Class(RecordFieldDeclaration)
		tagvalue:Expression;
		fields:RecordType ;

		Constructor Create(tagvalue:Expression ; fields:DeclarationList);	// type must be later set to the variant type
		{
			this.tagvalue = tagvalue;
			this.fields = new RecordType(fields);
		}
	End;

  ExportItem = Class(UnitItem)
		formalparams:ParametersSection;
		exportname:AnsiString;
		index:Integer;

    Constructor Create(name:StringObject; pars:ParametersSection); Overload;
		Constructor Create(name:StringObject; pars:ParametersSection; exportname:StringObject); Overload;
    Constructor Create(name:StringObject; pars:ParametersSection; Index:Integer); Overload;
  End;

	UnresolvedType = Class(TypeNode)
		id:StringObject;

		Constructor Create(id:StringObject);
  End;

	UnresolvedVariableType = Class(TypeNode)
		id:StringObject;

		Constructor Create(id:StringObject);
  End;

	UnresolvedIntegralType = Class(IntegralType)
		id:StringObject;

		Constructor Create(id:StringObject);
  End;


	UnresolvedOrdinalType = Class(TypeNode)
		id:StringObject;

		Constructor Create(id:StringObject);
  End;

  StringList = List;
  TypeList = List;


Function MakeThreadVars(Src:DeclarationList):DeclarationList;
Var
  I:Integer;
  VarDecl:VarDeclaration;
Begin
  For I:=0 To Pred(Src.Count) Do
  Begin
    VarDeclaration(Src.Declarations[I]).IsThrVar := True;
  End;

  Result := Src;
End;

Function CreateDecls(Ids:StringList; VarTypes:TypeNode; Init:Expression; AbsoluteID:StringObject):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  V:VarDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    V := VarDeclaration.Create(S, VarTypes, Init, AbsoluteID);
    Result.Add(V);
  End;
  It.Destroy;
End;

Function CreateLabelDecls(Ids:StringList):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  L:LabelDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    L := LabelDeclaration.Create(S, Nil);
    Result.Add(L);
  End;
  It.Destroy;
End;

Function CreateParamDecls(Ids:StringList; ParamType:TypeNode; Init:Expression; Kind:ParameterKind):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  P:ParamDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    P := ParamDeclaration.Create(S, ParamType, Init, Kind);
    Result.Add(P);
  End;
  It.Destroy;
End;

Function CreateVarParamDecls(Ids:StringList; ParamType:TypeNode):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  P:ParamDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    P := VarParamDeclaration.Create(S, ParamType);
    Result.Add(P);
  End;
  It.Destroy;
End;

Function CreateConstParamDecls(Ids:StringList; ParamType:TypeNode; init:Expression = Nil):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  P:ParamDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    P := ConstParamDeclaration.Create(S, ParamType, Init);
    Result.Add(P);
  End;
  It.Destroy;
End;

Function CreateFieldDecls(Ids:StringList; FieldsType:TypeNode):DeclarationList;
Var
  It:Iterator;
  S:StringObject;
  P:FieldDeclaration;
Begin
  Result := DeclarationList.Create();
  It := Ids.CreateIterator();
  While It.HasNext Do
  Begin
    S := StringObject(It.HasNext());
    P := FieldDeclaration.Create(S, FieldsType);
    Result.Add(P);
  End;
  It.Destroy;
End;

Function JoinImportDirectives(d1, d2:FunctionDirectiveList; i:FunctionDirective):ImportDirectives; Overload;
Begin
  Result := ImportDirectives.Create(i);
  Result.Add(d1);
	Result.Add(d2);
End;

Function JoinImportDirectives(d1, d2:FunctionDirectiveList; e:ExternalDirective):ImportDirectives; Overload;
Begin
  Result := JoinImportDirectives(d1, d2, importDirective_External);
  Result._External := e;
End;

Function CreateRecordUnionField(Src:ExpressionList; Fields:DeclarationList):DeclarationList;
Var
  I:Integer;
Begin
  Result := DeclarationList.Create();
  For I:=0 To Pred(Src.Count) Do
  Begin
    Result.Add(VarEntryDeclaration.Create(Src.Expressions[I], Fields));
  End;
End;

Procedure MakeFieldDeclarationsStatic(DeclList:DeclarationList);
Var
  I:Integer;
Begin
  For I:=0 To Pred(DeclList.Count) Do
    FieldDeclaration(DeclList.Declarations[I]).IsStatic := True;
End;

Function CheckDirectiveId(expected:AnsiString; idtoken:StringObject):Boolean;
Begin
  If (expected <> idtoken.Value) Then
  Begin
    RaiseError('Invalid directive ' + idtoken.Value + ', expected: ' + expected);
    Result := False;
  End Else
    Result := True;
End;

Function CreateBinaryExpression(e1:Expression; token:Integer; e2:Expression):BinaryExpression; Forward;

{$I delphi_parser.inc}


Function CreateBinaryExpression(e1:Expression; token:Integer; e2:Expression):BinaryExpression;
Begin
  Case token Of
				KW_MUL: Result := Product.Create(e1, e2);
				KW_DIV: Result := Division.Create(e1, e2);
				KW_QUOT:Result := Quotient.Create(e1, e2);
				KW_MOD: Result := Modulus.Create(e1, e2);
				KW_SHR: Result := ShiftRight.Create(e1, e2);
				KW_SHL: Result := ShiftLeft.Create(e1, e2);
				KW_AND: Result := LogicalAnd.Create(e1, e2);
				KW_SUB: Result := Subtraction.Create(e1, e2);
				KW_SUM: Result := Addition.Create(e1, e2);
				KW_OR : Result := LogicalOr.Create(e1, e2);
				KW_XOR: Result := LogicalXor.Create(e1, e2);
				KW_EQ : Result := Equal.Create(e1, e2);
				KW_NE : Result := NotEqual.Create(e1, e2);
				KW_LT : Result := LessThan.Create(e1, e2);
				KW_LE : Result := LessOrEqual.Create(e1, e2);
				KW_GT : Result := GreaterThan.Create(e1, e2);
				KW_GE : Result := GreaterOrEqual.Create(e1, e2);
    Else
      RaiseError('Invalid Binary Operation token: ' + IntToString(token));
  End;
End;


{ PascalLexer }
Constructor PascalLexer.Create(Source: Stream);
Begin
  Inherited Create;

  _StateIndex := 0;
  _Source := Source;
End;

Function PascalLexer.get_char: AnsiChar;
Var
  I:Integer;
Begin
  If (_bufptr = 0) And (Not _Source.EOF) Then
  Begin
    _Source.ReadLine(yyline);
    Inc(yylineno);
    yycolno := 1;
    _Buf[1]  := nl;

    For i := 1 To length(yyline) Do
      _Buf[i + 1] := yyline[length(yyline) - i + 1];

    Inc(_Bufptr, length(yyline) + 1);
  end;

  If (_Bufptr > 0) Then
  Begin
    Result := _Buf[_Bufptr];
    Dec(_Bufptr);
    Inc(yycolno);
  End Else
    Result := #0;
End;

Procedure PascalLexer.yybegin(newState:Integer);
Begin
  _CurrentState := newState;
End;

Procedure PascalLexer.PopState();
Begin
  If (_StateIndex>0) Then
  Begin
    //Result := _StateBuffer[_StateIndex];
    yybegin(_StateBuffer[_StateIndex]);
    Dec(_StateIndex);
  End;
End;

Procedure PascalLexer.pushstate(State: Integer);
Begin
  Inc(_StateIndex);
  _StateBuffer[_StateIndex] := State;
  yybegin(State);
End;

Procedure PascalLexer.switchCallback(Callback, GotoState: Integer);
Begin
  PopState();
  PushState(Callback);
  PushState(GotoState);
  yybegin(GotoState);
End;

Procedure PascalLexer.switchstate(State: Integer);
Begin
  PopState();
	PushState(state);
	yybegin(state);
End;

Procedure PascalLexer.unget_char(c: AnsiChar);
Begin
  If _Bufptr = max_chars Then
  Begin
    yyerror('input buffer overflow');
    Exit;
  End;

  Inc(_bufptr);
  Dec(yycolno);
  _Buf[_Bufptr] := c;
End;

Procedure PascalLexer.yyclear();
Begin
  Inherited;
  _bufptr := 0;
End;

// If `yywrap()' returns false (zero), then it is assumed that the function has gone ahead and set up yyin to point to another input file, and scanning continues.
// If it returns true (non-zero), then the scanner terminates, returning 0 to its caller.
Procedure PascalLexer.yyerror(const S: AnsiString);
Begin
  WriteLn(S);
  ReadLn;
End;

Function PascalLexer.yylaststate: Integer;
Begin
  If (_StateIndex>1) Then
    Result := _StateBuffer[Pred(_StateIndex)]
  Else
    Result := 0;
End;

Function PascalLexer.yylength:Integer;
Begin
  //Result := Length(yytext);
  Result := yysleng;
End;

Procedure PascalLexer.yypushback(Count: Integer);
Begin
  Dec(_bufptr, Count);

  If (_BufPtr<0) Then
  Begin
    YYError('pushback error!');
  End;
End;

Function PascalLexer.yywrap:Boolean;
Begin
  DestroyObject(@Self._Source);
  Result := True;
End;

Function GetDirectiveArg(directive:AnsiString):AnsiString;
Begin
  Result := Copy(Directive, 3, Length(directive)-3);
End;

Function ProcessIdentifier(id:AnsiString):Integer;
Begin
  //yylval := id;
	Result := PASCAL_IDENTIFIER;
End;

Destructor PascalLexer.Destroy;
Begin
  DestroyObject(@_Source);
End;

{$I delphi_lexer.inc}

// nodes implementation

{ MetaType }
Constructor MetaType.Create(TypeClass: MetaTypeClass);
Begin
  Self.Value := TypeClass;
End;

{ Declaration }
Constructor Declaration.Create(Name: StringObject; T: TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := T;
End;

{ DeclarationList }
Constructor DeclarationList.Create(Decl: Declaration);
Begin
  Self.Count := 0;
  Self.Add(Decl);
End;

Procedure DeclarationList.Add(Decl: Declaration);
Begin
  If Decl = Nil Then
    Exit;

  Inc(Count);
  SetLength(Declarations, Count);
  Declarations[Pred(Count)] := Decl;
End;

Procedure DeclarationList.Add(DeclList: DeclarationList);
Var
  I:Integer;
Begin
  If DeclList = Nil Then
    Exit;

  For I:=0 To Pred(DeclList.Count) Do
    Self.Add(DeclList.Declarations[I]);
End;

Procedure DeclarationList.InsertAt(Index:Integer; DeclList:DeclarationList);
Var
  I, Prev:Integer;
Begin
  If DeclList = Nil Then
    Exit;

  If (Index>=Count) Then
  Begin
    Self.Add(DeclList);
    Exit;
  End;

  Prev := Count;
  Inc(Count, DeclList.Count);
  SetLength(Declarations, Count);

  For I:=Index To Pred(Prev) Do
    Declarations[I+DeclList.Count] := Declarations[I];

  For I:=0 To Pred(DeclList.Count) Do
    Declarations[I+Prev] := DeclList.Declarations[I];
End;

{ UnitItem }
Constructor UnitItem.Create(Name, Location: StringObject);
Begin
  Self.Name := Name;
  Self.Location := Location;
  Self.DeclType := Nil;
End;

Constructor UnitItem.Create(Name: StringObject);
Begin
  Self.Create(Name, Nil);
End;

{ ProgramNode }
Constructor ProgramNode.Create(Name:StringObject; UsesList:NodeList; Decls:DeclarationList; Body:BlockStatement);
Begin
  Self.Name := Name;
  Self.Section := ProgramSection.Create(UsesList, Decls, Body);
End;

{ CompositeType }
Function CompositeType.IsForward: Boolean;
Begin
  Result := (Section = Nil);
End;

{ Section }
Constructor Section.Create(Decls: DeclarationList);
Begin
  Self.Decls := Decls;
End;

{ TopLevelDeclarationSection }
Constructor TopLevelDeclarationSection.Create(UsesList:NodeList; Decls:DeclarationList);
Begin
  Self.Useslist := UsesList;
  Self.Decls := Decls;
End;

{ StatementList }
Constructor StatementList.Create(St: Statement);
Begin
  Self.Count := 0;
  Self.Add(St);
End;

Procedure StatementList.Add(St: Statement);
Begin
  If St = Nil Then
    Exit;

  Inc(Count);
  SetLength(Statements, Count);

  Statements[Pred(Count)] := St;
End;

{ BlockStatement }

Constructor BlockStatement.Create(List: StatementList);
Begin
  Self.List := List;
End;

{ ProgramSection }
Constructor ProgramSection.Create(UsesList: NodeList; Decls: DeclarationList; Code: BlockStatement);
Begin
  Self.Block := Code;
  Self.Useslist := UsesList;
  Self.Decls := Decls;
End;

{ ExpressionList }
Constructor ExpressionList.Create(Exp: Expression);
Begin
  Self.Count := 0;
  Self.Add(Exp);
End;

Procedure ExpressionList.Add(Exp: Expression);
Begin
  If Exp = Nil Then
    Exit;

  Inc(Count);
  SetLength(Expressions, Count);
  Expressions[Pred(Count)] := Exp;
End;

Procedure ExpressionList.InsertAt(Index:Integer; Exp:Expression);
Var
  Prev, I:Integer;
Begin
  If (Exp = Nil) Then
    Exit;

  If (Index>=Count) Then
  Begin
    Self.Add(Exp);
    Exit;
  End;

  Prev := Count;
  Inc(Count);
  For I:=Pred(Prev) DownTo Index Do
    Expressions[I+1] := Expressions[I];

  Expressions[Index] := Exp;
End;

{ ConstExpression }
Function ConstExpression.ResolveToLiteral: Literal;
Begin
  Result := Nil; //TODO
End;

{ VarDeclaration }
Constructor VarDeclaration.Create(Name:StringObject; VarType:TypeNode; Init:Expression; AbsoluteId:StringObject);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;
  Self.Init := Init;
  Self.AbsoluteID := AbsoluteID;
End;

Constructor VarDeclaration.Create(Name:StringObject; VarType:TypeNode; Init:Expression);
Begin
  Self.Create(Name, VarType, Init, Nil);
End;

{ ParamDeclaration }
Constructor ParamDeclaration.Create(Name:StringObject; ParamType:TypeNode; Init:Expression; Kind:ParameterKind);
Begin
  Self.Name := Name;
  Self.DeclType := ParamType;
  Self.Init := Init;
  Self.Kind := Kind;
End;

{ VarParamDeclaration }
Constructor VarParamDeclaration.Create(Name:StringObject; VarType:TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;
End;

{ ConstParamDeclaration }
Constructor ConstParamDeclaration.Create(Name:StringObject; ConstType:TypeNode; Init:Expression);
Begin
  Self.Name := Name;
  Self.DeclType := ConstType;
  Self.Init := Init;
End;

{ OutParamDeclaration }
Constructor OutParamDeclaration.Create(Name:StringObject; VarType:TypeNode);
Begin
  Self.Name := Name;
  Self.DeclType := VarType;
End;

{ ParametersSection }
Constructor ParametersSection.Create(Decls: DeclarationList);
Begin
  Self.Decls := Decls;
End;

{ FunctionDirectiveList }
sdfsfs
constructor FunctionDirectiveList.Create(Dirs: FunctionDirective);
begin

end;

Procedure FunctionDirectiveList.Add(dirs: FunctionDirective);
Begin

end;

function FunctionDirectiveList.CheckDirectives: Boolean;
begin

end;

function FunctionDirectiveList.Contains(dir: FunctionDirective): Boolean;
begin

end;


Begin
End.
