Unit XPC_Delphi;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, XPC_Lexer, XPC_Parser;

Type
	IntegerLiteral1RegexMatcher = Class(RegexTokenMatcher) // matcher for [+-]?[0-9]+
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	IntegerLiteral2RegexMatcher = Class(RegexTokenMatcher) // matcher for [+-]?[$][0-9a-f]*
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	FloatLiteral1RegexMatcher = Class(RegexTokenMatcher) // matcher for [+-]?[0-9]+.[0-9]+
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	FloatLiteral2RegexMatcher = Class(RegexTokenMatcher) // matcher for [+-]?[0-9]+e[+-]?[0-9]+
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	FloatLiteral3RegexMatcher = Class(RegexTokenMatcher) // matcher for [+-]?[0-9]+.[0-9]+e[+-]?[0-9]+
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	StringLiteralRegexMatcher = Class(RegexTokenMatcher) // matcher for '[^']*'
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	CharLiteralRegexMatcher = Class(RegexTokenMatcher) // matcher for [#][0-9]+
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	IdentifierRegexMatcher = Class(RegexTokenMatcher) // matcher for [_a-zA-Z][0-9a-zA-Z_]*
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;

	WhiteSpaceRegexMatcher = Class(RegexTokenMatcher) // matcher for [ \n\t]*
		Function Match(Const C:TERRAChar):Boolean; Override;
	End;


	GoalNode = Class(ASTNode)
	End;

	ExpressionNode = Class(ASTNode)
	End;

	LiteralNode = Class(ExpressionNode)
	End;

	BoolLiteralNode = Class(LiteralNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	IntegerLiteralNode = Class(LiteralNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	FloatLiteralNode = Class(LiteralNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	StringLiteralNode = Class(LiteralNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	CharLiteralNode = Class(LiteralNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	IdentifierNode = Class(ExpressionNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	IdentifierNodeArray = Array Of IdentifierNode;

	IdentifierListNode = Class(ASTNode)
		Public
			Identifiers: IdentifierNodeArray;

			Constructor Create(Identifiers:IdentifierNodeArray);
	End;

	WhiteSpaceNode = Class(ASTNode)
		Public
			Value: TERRAString;

			Constructor Create(Const Value:TERRAString);
			Function GetValue():TERRAString; Override;
	End;

	UsesDeclNode = Class(ASTNode)
		Public
			IdentifierList: IdentifierListNode;

			Constructor Create(IdentifierList:IdentifierListNode);
	End;

	DeclarationsNode = Class(ASTNode)
	End;

	StatementNode = Class(ASTNode)
	End;

	StatementNodeArray = Array Of StatementNode;

	BlockNode = Class(StatementNode)
		Public
			Statements: StatementNodeArray;

			Constructor Create(Statements:StatementNodeArray);
	End;

	DeclarationsNodeArray = Array Of DeclarationsNode;

	ProgramNode = Class(GoalNode)
		Public
			Identifier: IdentifierNode;
			UsesDecl: UsesDeclNode;
			Declarationss: DeclarationsNodeArray;
			Block: BlockNode;

			Constructor Create(Identifier:IdentifierNode; UsesDecl:UsesDeclNode; Declarationss:DeclarationsNodeArray; Block:BlockNode);
	End;

	ConstDeclarationNode = Class(ASTNode)
		Public
			Identifier: IdentifierNode;
			Literal: LiteralNode;

			Constructor Create(Identifier:IdentifierNode; Literal:LiteralNode);
	End;

	ConstDeclarationNodeArray = Array Of ConstDeclarationNode;

	ConstsNode = Class(DeclarationsNode)
		Public
			ConstDeclarations: ConstDeclarationNodeArray;

			Constructor Create(ConstDeclarations:ConstDeclarationNodeArray);
	End;

	VarDeclarationNode = Class(ASTNode)
		Public
			IdentifierList: IdentifierListNode;
			TypeID: IdentifierNode;

			Constructor Create(IdentifierList:IdentifierListNode; TypeID:IdentifierNode);
	End;

	VarDeclarationNodeArray = Array Of VarDeclarationNode;

	VarsNode = Class(DeclarationsNode)
		Public
			VarDeclarations: VarDeclarationNodeArray;

			Constructor Create(VarDeclarations:VarDeclarationNodeArray);
	End;

	TypeDeclarationNode = Class(ASTNode)
	End;

	TypeDeclarationNodeArray = Array Of TypeDeclarationNode;

	TypesNode = Class(DeclarationsNode)
		Public
			TypeDeclarations: TypeDeclarationNodeArray;

			Constructor Create(TypeDeclarations:TypeDeclarationNodeArray);
	End;

	TypeAliasNode = Class(TypeDeclarationNode)
		Public
			Identifier: IdentifierNode;
			Alias: IdentifierNode;

			Constructor Create(Identifier:IdentifierNode; Alias:IdentifierNode);
	End;


	RecordTypeNode = Class(TypeDeclarationNode)
		Public
			Identifier: IdentifierNode;
			IsPacked: Boolean;
			VarDeclarations: VarDeclarationNodeArray;

			Constructor Create(Identifier:IdentifierNode; Const IsPacked:Boolean; VarDeclarations:VarDeclarationNodeArray);
	End;

	EnumNode = Class(ASTNode)
		Public
			Identifier: IdentifierNode;
			IntegerLiteral: IntegerLiteralNode;

			Constructor Create(Identifier:IdentifierNode; IntegerLiteral:IntegerLiteralNode);
	End;

	EnumNodeArray = Array Of EnumNode;

	EnumListNode = Class(ASTNode)
		Public
			Enums: EnumNodeArray;

			Constructor Create(Enums:EnumNodeArray);
	End;

	EnumerationNode = Class(TypeDeclarationNode)
		Public
			Identifier: IdentifierNode;
			EnumList: EnumListNode;

			Constructor Create(Identifier:IdentifierNode; EnumList:EnumListNode);
	End;

	RangeNode = Class(ExpressionNode)
		Public
			RangeStart: LiteralNode;
			RangeEnd: LiteralNode;

			Constructor Create(RangeStart:LiteralNode; RangeEnd:LiteralNode);
	End;

	RangeTypeNode = Class(TypeDeclarationNode)
		Public
			Identifier: IdentifierNode;
			Range: RangeNode;

			Constructor Create(Identifier:IdentifierNode; Range:RangeNode);
	End;

	RangeNodeArray = Array Of RangeNode;

	RangeListNode = Class(ASTNode)
		Public
			Ranges: RangeNodeArray;

			Constructor Create(Ranges:RangeNodeArray);
	End;

	ArrayNode = Class(ASTNode)
		Public
			RangeList: RangeListNode;
			Identifier: IdentifierNode;

			Constructor Create(RangeList:RangeListNode; Identifier:IdentifierNode);
	End;

	ArrayTypeNode = Class(TypeDeclarationNode)
		Public
			Identifier: IdentifierNode;
			ArrayValue: ArrayNode;

			Constructor Create(Identifier:IdentifierNode; ArrayValue:ArrayNode);
	End;

	RoutineDeclarationNode = Class(ASTNode)
	End;

	RoutineArgumentDeclarationNode = Class(ASTNode)
		Public
			IdentifierList: IdentifierListNode;
			TypeID: IdentifierNode;

			Constructor Create(IdentifierList:IdentifierListNode; TypeID:IdentifierNode);
	End;

	RoutineArgumentDeclarationNodeArray = Array Of RoutineArgumentDeclarationNode;

	RoutineArgumentsNode = Class(ASTNode)
		Public
			RoutineArgumentDeclarations: RoutineArgumentDeclarationNodeArray;

			Constructor Create(RoutineArgumentDeclarations:RoutineArgumentDeclarationNodeArray);
	End;

	RoutineReturnTypeNode = Class(ASTNode)
		Public
			Identifier: IdentifierNode;

			Constructor Create(Identifier:IdentifierNode);
	End;


	FunctionsNode = Class(DeclarationsNode)
		Public
			RoutineDeclaration: RoutineDeclarationNode;
			RoutineArguments: RoutineArgumentsNode;
			RoutineReturnType: RoutineReturnTypeNode;
			Declarationss: DeclarationsNodeArray;
			Block: BlockNode;

			Constructor Create(RoutineDeclaration:RoutineDeclarationNode; RoutineArguments:RoutineArgumentsNode; RoutineReturnType:RoutineReturnTypeNode; Declarationss:DeclarationsNodeArray; Block:BlockNode);
	End;

	ProcedureDeclarationNode = Class(RoutineDeclarationNode)
		Public
			Identifier: IdentifierNode;

			Constructor Create(Identifier:IdentifierNode);
	End;

	FunctionDeclarationNode = Class(RoutineDeclarationNode)
		Public
			Identifier: IdentifierNode;

			Constructor Create(Identifier:IdentifierNode);
	End;

	AssignmentNode = Class(StatementNode)
		Public
			Identifier: IdentifierNode;
			Expression: ExpressionNode;

			Constructor Create(Identifier:IdentifierNode; Expression:ExpressionNode);
	End;


	WhileStatementNode = Class(StatementNode)
		Public
			Condition: ExpressionNode;
			Statements: StatementNodeArray;

			Constructor Create(Condition:ExpressionNode; Statements:StatementNodeArray);
	End;


	RepeatStatementNode = Class(StatementNode)
		Public
			Statements: StatementNodeArray;
			Condition: ExpressionNode;

			Constructor Create(Statements:StatementNodeArray; Condition:ExpressionNode);
	End;


	ForStatementNode = Class(StatementNode)
		Public
			Assignment: AssignmentNode;
			Expression: ExpressionNode;
			Statements: StatementNodeArray;

			Constructor Create(Assignment:AssignmentNode; Expression:ExpressionNode; Statements:StatementNodeArray);
	End;

	IfStatementNode = Class(StatementNode)
		Public
			Expression: ExpressionNode;
			TrueStatement: StatementNode;
			FalseStatement: StatementNode;

			Constructor Create(Expression:ExpressionNode; TrueStatement:StatementNode; FalseStatement:StatementNode);
	End;

	ExpressionNodeArray = Array Of ExpressionNode;

	ExpressionListNode = Class(ASTNode)
		Public
			Expressions: ExpressionNodeArray;

			Constructor Create(Expressions:ExpressionNodeArray);
	End;

	FunctionCallNode = Class(ExpressionNode)
		Public
			Identifier: IdentifierNode;
			ExpressionList: ExpressionListNode;

			Constructor Create(Identifier:IdentifierNode; ExpressionList:ExpressionListNode);
	End;

	PriorityExpressionNode = Class(ExpressionNode)
		Public
			Expression: ExpressionNode;

			Constructor Create(Expression:ExpressionNode);
	End;

	UnaryOperatorNode = Class(ASTNode)
	End;

	UnaryExpressionNode = Class(ExpressionNode)
		Public
			UnaryOperator: UnaryOperatorNode;
			Expression: ExpressionNode;

			Constructor Create(UnaryOperator:UnaryOperatorNode; Expression:ExpressionNode);
	End;

	UnaryOperatorPlusNode = Class(UnaryOperatorNode)
		Public

			Constructor Create();
	End;

	UnaryOperatorMinusNode = Class(UnaryOperatorNode)
		Public

			Constructor Create();
	End;

	UnaryOperatorAddressOfNode = Class(UnaryOperatorNode)
		Public

			Constructor Create();
	End;

	UnaryOperatorNotNode = Class(UnaryOperatorNode)
		Public

			Constructor Create();
	End;

	BinaryOperatorNode = Class(ASTNode)
	End;

	BinaryExpressionNode = Class(ExpressionNode)
		Public
			ExprA: ExpressionNode;
			BinaryOperator: BinaryOperatorNode;
			ExprB: ExpressionNode;

			Constructor Create(ExprA:ExpressionNode; BinaryOperator:BinaryOperatorNode; ExprB:ExpressionNode);
	End;

	LogicBinaryOperatorNode = Class(BinaryOperatorNode)
	End;

	LogicBinaryExpressionNode = Class(ASTNode)
		Public
			ExprA: ExpressionNode;
			LogicBinaryOperator: LogicBinaryOperatorNode;
			ExprB: ExpressionNode;

			Constructor Create(ExprA:ExpressionNode; LogicBinaryOperator:LogicBinaryOperatorNode; ExprB:ExpressionNode);
	End;

	LogicBinaryOperatorEqualityNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorInequalityNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorAndNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorOrNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorXorNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorGreaterNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorGreaterOrEqualNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorLessNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorLessOrEqualNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	LogicBinaryOperatorInsideNode = Class(LogicBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorNode = Class(BinaryOperatorNode)
	End;

	ArithmeticBinaryOperatorPlusNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorMinusNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorDivisionNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorModulusNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorProductNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorShiftLeftNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;

	ArithmeticBinaryOperatorShiftRightNode = Class(ArithmeticBinaryOperatorNode)
		Public

			Constructor Create();
	End;


	DelphiLexer = Class(Lexer)
		Public
			Constructor Create();
	End;

	DelphiProcessor = Class(TERRAObject)
		Public
			Procedure VisitGoal(Node:GoalNode); Virtual;
			Procedure VisitExpression(Node:ExpressionNode); Virtual;
			Procedure VisitLiteral(Node:LiteralNode); Virtual;
			Procedure VisitBoolLiteral(Node:BoolLiteralNode); Virtual;
			Procedure VisitIntegerLiteral(Node:IntegerLiteralNode); Virtual;
			Procedure VisitFloatLiteral(Node:FloatLiteralNode); Virtual;
			Procedure VisitStringLiteral(Node:StringLiteralNode); Virtual;
			Procedure VisitCharLiteral(Node:CharLiteralNode); Virtual;
			Procedure VisitIdentifier(Node:IdentifierNode); Virtual;
			Procedure VisitIdentifierList(Node:IdentifierListNode); Virtual;
			Procedure VisitWhiteSpace(Node:WhiteSpaceNode); Virtual;
			Procedure VisitUsesDecl(Node:UsesDeclNode); Virtual;
			Procedure VisitDeclarations(Node:DeclarationsNode); Virtual;
			Procedure VisitStatement(Node:StatementNode); Virtual;
			Procedure VisitBlock(Node:BlockNode); Virtual;
			Procedure VisitProgram(Node:ProgramNode); Virtual;
			Procedure VisitConstDeclaration(Node:ConstDeclarationNode); Virtual;
			Procedure VisitConsts(Node:ConstsNode); Virtual;
			Procedure VisitVarDeclaration(Node:VarDeclarationNode); Virtual;
			Procedure VisitVars(Node:VarsNode); Virtual;
			Procedure VisitTypeDeclaration(Node:TypeDeclarationNode); Virtual;
			Procedure VisitTypes(Node:TypesNode); Virtual;
			Procedure VisitTypeAlias(Node:TypeAliasNode); Virtual;
			Procedure VisitRecordType(Node:RecordTypeNode); Virtual;
			Procedure VisitEnum(Node:EnumNode); Virtual;
			Procedure VisitEnumList(Node:EnumListNode); Virtual;
			Procedure VisitEnumeration(Node:EnumerationNode); Virtual;
			Procedure VisitRange(Node:RangeNode); Virtual;
			Procedure VisitRangeType(Node:RangeTypeNode); Virtual;
			Procedure VisitRangeList(Node:RangeListNode); Virtual;
			Procedure VisitArray(Node:ArrayNode); Virtual;
			Procedure VisitArrayType(Node:ArrayTypeNode); Virtual;
			Procedure VisitRoutineDeclaration(Node:RoutineDeclarationNode); Virtual;
			Procedure VisitRoutineArgumentDeclaration(Node:RoutineArgumentDeclarationNode); Virtual;
			Procedure VisitRoutineArguments(Node:RoutineArgumentsNode); Virtual;
			Procedure VisitRoutineReturnType(Node:RoutineReturnTypeNode); Virtual;
			Procedure VisitFunctions(Node:FunctionsNode); Virtual;
			Procedure VisitProcedureDeclaration(Node:ProcedureDeclarationNode); Virtual;
			Procedure VisitFunctionDeclaration(Node:FunctionDeclarationNode); Virtual;
			Procedure VisitAssignment(Node:AssignmentNode); Virtual;
			Procedure VisitWhileStatement(Node:WhileStatementNode); Virtual;
			Procedure VisitRepeatStatement(Node:RepeatStatementNode); Virtual;
			Procedure VisitForStatement(Node:ForStatementNode); Virtual;
			Procedure VisitIfStatement(Node:IfStatementNode); Virtual;
			Procedure VisitExpressionList(Node:ExpressionListNode); Virtual;
			Procedure VisitFunctionCall(Node:FunctionCallNode); Virtual;
			Procedure VisitPriorityExpression(Node:PriorityExpressionNode); Virtual;
			Procedure VisitUnaryOperator(Node:UnaryOperatorNode); Virtual;
			Procedure VisitUnaryExpression(Node:UnaryExpressionNode); Virtual;
			Procedure VisitUnaryOperatorPlus(Node:UnaryOperatorPlusNode); Virtual;
			Procedure VisitUnaryOperatorMinus(Node:UnaryOperatorMinusNode); Virtual;
			Procedure VisitUnaryOperatorAddressOf(Node:UnaryOperatorAddressOfNode); Virtual;
			Procedure VisitUnaryOperatorNot(Node:UnaryOperatorNotNode); Virtual;
			Procedure VisitBinaryOperator(Node:BinaryOperatorNode); Virtual;
			Procedure VisitBinaryExpression(Node:BinaryExpressionNode); Virtual;
			Procedure VisitLogicBinaryOperator(Node:LogicBinaryOperatorNode); Virtual;
			Procedure VisitLogicBinaryExpression(Node:LogicBinaryExpressionNode); Virtual;
			Procedure VisitLogicBinaryOperatorEquality(Node:LogicBinaryOperatorEqualityNode); Virtual;
			Procedure VisitLogicBinaryOperatorInequality(Node:LogicBinaryOperatorInequalityNode); Virtual;
			Procedure VisitLogicBinaryOperatorAnd(Node:LogicBinaryOperatorAndNode); Virtual;
			Procedure VisitLogicBinaryOperatorOr(Node:LogicBinaryOperatorOrNode); Virtual;
			Procedure VisitLogicBinaryOperatorXor(Node:LogicBinaryOperatorXorNode); Virtual;
			Procedure VisitLogicBinaryOperatorGreater(Node:LogicBinaryOperatorGreaterNode); Virtual;
			Procedure VisitLogicBinaryOperatorGreaterOrEqual(Node:LogicBinaryOperatorGreaterOrEqualNode); Virtual;
			Procedure VisitLogicBinaryOperatorLess(Node:LogicBinaryOperatorLessNode); Virtual;
			Procedure VisitLogicBinaryOperatorLessOrEqual(Node:LogicBinaryOperatorLessOrEqualNode); Virtual;
			Procedure VisitLogicBinaryOperatorInside(Node:LogicBinaryOperatorInsideNode); Virtual;
			Procedure VisitArithmeticBinaryOperator(Node:ArithmeticBinaryOperatorNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorPlus(Node:ArithmeticBinaryOperatorPlusNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorMinus(Node:ArithmeticBinaryOperatorMinusNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorDivision(Node:ArithmeticBinaryOperatorDivisionNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorModulus(Node:ArithmeticBinaryOperatorModulusNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorProduct(Node:ArithmeticBinaryOperatorProductNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorShiftLeft(Node:ArithmeticBinaryOperatorShiftLeftNode); Virtual;
			Procedure VisitArithmeticBinaryOperatorShiftRight(Node:ArithmeticBinaryOperatorShiftRightNode); Virtual;
	End;

	DelphiParser = Class(Parser)
		Protected
			Function ParseGoal():ASTNode; Override;
			Function ParseExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):ExpressionNode;
			Function ParseLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):LiteralNode;
			Function ParseBoolLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):BoolLiteralNode;
			Function ParseIntegerLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):IntegerLiteralNode;
			Function ParseFloatLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):FloatLiteralNode;
			Function ParseStringLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):StringLiteralNode;
			Function ParseCharLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):CharLiteralNode;
			Function ParseIdentifier(HandlerNode:ASTNodeClass; IsOpt:Boolean):IdentifierNode;
			Function ParseIdentifierList(HandlerNode:ASTNodeClass; IsOpt:Boolean):IdentifierListNode;
			Function ParseWhiteSpace(HandlerNode:ASTNodeClass; IsOpt:Boolean):WhiteSpaceNode;
			Function ParseUsesDecl(HandlerNode:ASTNodeClass; IsOpt:Boolean):UsesDeclNode;
			Function ParseDeclarations(HandlerNode:ASTNodeClass; IsOpt:Boolean):DeclarationsNode;
			Function ParseStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):StatementNode;
			Function ParseBlock(HandlerNode:ASTNodeClass; IsOpt:Boolean):BlockNode;
			Function ParseProgram(HandlerNode:ASTNodeClass; IsOpt:Boolean):ProgramNode;
			Function ParseConstDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):ConstDeclarationNode;
			Function ParseConsts(HandlerNode:ASTNodeClass; IsOpt:Boolean):ConstsNode;
			Function ParseVarDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):VarDeclarationNode;
			Function ParseVars(HandlerNode:ASTNodeClass; IsOpt:Boolean):VarsNode;
			Function ParseTypeDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypeDeclarationNode;
			Function ParseTypes(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypesNode;
			Function ParseTypeAlias(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypeAliasNode;
			Function ParseRecordType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RecordTypeNode;
			Function ParseEnum(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumNode;
			Function ParseEnumList(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumListNode;
			Function ParseEnumeration(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumerationNode;
			Function ParseRange(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeNode;
			Function ParseRangeType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeTypeNode;
			Function ParseRangeList(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeListNode;
			Function ParseArray(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArrayNode;
			Function ParseArrayType(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArrayTypeNode;
			Function ParseRoutineDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineDeclarationNode;
			Function ParseRoutineArgumentDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineArgumentDeclarationNode;
			Function ParseRoutineArguments(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineArgumentsNode;
			Function ParseRoutineReturnType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineReturnTypeNode;
			Function ParseFunctions(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionsNode;
			Function ParseProcedureDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):ProcedureDeclarationNode;
			Function ParseFunctionDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionDeclarationNode;
			Function ParseAssignment(HandlerNode:ASTNodeClass; IsOpt:Boolean):AssignmentNode;
			Function ParseWhileStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):WhileStatementNode;
			Function ParseRepeatStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):RepeatStatementNode;
			Function ParseForStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):ForStatementNode;
			Function ParseIfStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):IfStatementNode;
			Function ParseExpressionList(HandlerNode:ASTNodeClass; IsOpt:Boolean):ExpressionListNode;
			Function ParseFunctionCall(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionCallNode;
			Function ParsePriorityExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):PriorityExpressionNode;
			Function ParseUnaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorNode;
			Function ParseUnaryExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryExpressionNode;
			Function ParseUnaryOperatorPlus(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorPlusNode;
			Function ParseUnaryOperatorMinus(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorMinusNode;
			Function ParseUnaryOperatorAddressOf(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorAddressOfNode;
			Function ParseUnaryOperatorNot(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorNotNode;
			Function ParseBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):BinaryOperatorNode;
			Function ParseLogicBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorNode;
			Function ParseLogicBinaryExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryExpressionNode;
			Function ParseLogicBinaryOperatorEquality(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorEqualityNode;
			Function ParseLogicBinaryOperatorInequality(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorInequalityNode;
			Function ParseLogicBinaryOperatorAnd(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorAndNode;
			Function ParseLogicBinaryOperatorOr(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorOrNode;
			Function ParseLogicBinaryOperatorXor(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorXorNode;
			Function ParseLogicBinaryOperatorGreater(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorGreaterNode;
			Function ParseLogicBinaryOperatorGreaterOrEqual(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorGreaterOrEqualNode;
			Function ParseLogicBinaryOperatorLess(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorLessNode;
			Function ParseLogicBinaryOperatorLessOrEqual(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorLessOrEqualNode;
			Function ParseLogicBinaryOperatorInside(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorInsideNode;
			Function ParseArithmeticBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorNode;
			Function ParseArithmeticBinaryOperatorPlus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorPlusNode;
			Function ParseArithmeticBinaryOperatorMinus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorMinusNode;
			Function ParseArithmeticBinaryOperatorDivision(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorDivisionNode;
			Function ParseArithmeticBinaryOperatorModulus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorModulusNode;
			Function ParseArithmeticBinaryOperatorProduct(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorProductNode;
			Function ParseArithmeticBinaryOperatorShiftLeft(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorShiftLeftNode;
			Function ParseArithmeticBinaryOperatorShiftRight(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorShiftRightNode;
			Function GetTokenName(ID:Cardinal; Const Value:TERRAString):TERRAString; Override;
		Public
			Function Parse(Source:Stream; IgnoreCase:Boolean):ASTNode; Override;
	End;

Implementation
Uses TERRA_Error;

Function IntegerLiteral1RegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		1:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		2:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function IntegerLiteral2RegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		1:
			If (C = 36) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		2:
			If ((C >= 48) And (C <= 57)) Or ((C >= 97) And (C <= 102)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function FloatLiteral1RegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		1:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		2:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		3:
			If (C = 46) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		4:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		5:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function FloatLiteral2RegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		1:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		2:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		3:
			If (C = 101) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		4:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		5:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		6:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function FloatLiteral3RegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		1:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		2:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		3:
			If (C = 46) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		4:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		5:
			If ((C >= 48) And (C <= 57)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		6:
			If (C = 101) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		7:
			If ((C >= 43) And (C <= 45)) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		8:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		9:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function StringLiteralRegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If (C = 39) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		1:
			If Not ((C = 39)) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		2:
			If (C = 39) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		Else
			Result := False;
	End;
End;

Function CharLiteralRegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If (C = 35) Then
			Begin
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		1:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
			Begin
				Result := False;
			End;
		2:
			If ((C >= 48) And (C <= 57)) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function IdentifierRegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If (C = 95) Or ((C >= 97) And (C <= 122)) Or ((C >= 65) And (C <= 90)) Then
			Begin
			_Complete := True;
				Result := True;
				Inc(_State);
			End Else
				Result := False;

		1:
			If ((C >= 48) And (C <= 57)) Or ((C >= 97) And (C <= 122)) Or ((C >= 65) And (C <= 90)) Or (C = 95) Then
			Begin
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Function WhiteSpaceRegexMatcher.Match(Const C:TERRAChar):Boolean;
Begin
	Case _State Of
		0:
			If (C = 32) Or (C = 13) Or (C = 9) Then
			Begin
			_Complete := True;
				Result := True;
			End Else
			Begin
				Inc(_State);
				Result := Self.Match(C);
			End;

		Else
			Result := False;
	End;
End;

Constructor BoolLiteralNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function BoolLiteralNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor IntegerLiteralNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function IntegerLiteralNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor FloatLiteralNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function FloatLiteralNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor StringLiteralNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function StringLiteralNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor CharLiteralNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function CharLiteralNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor IdentifierNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function IdentifierNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor IdentifierListNode.Create(Identifiers:IdentifierNodeArray);
Var
	I:Integer;
Begin
	Self.Identifiers := Identifiers;
	If Assigned(Identifiers) Then
		For I:=0 To Pred(Length(Identifiers)) Do
			Identifiers[I].SetParent(Self);
End;

Constructor WhiteSpaceNode.Create(Const Value:TERRAString);
Begin
	Self.Value := Value;
End;

Function WhiteSpaceNode.GetValue():TERRAString;
Begin
	Result := Value;
End;

Constructor UsesDeclNode.Create(IdentifierList:IdentifierListNode);
Begin
	Self.IdentifierList := IdentifierList;
	If Assigned(IdentifierList) Then
		IdentifierList.SetParent(Self);
End;

Constructor BlockNode.Create(Statements:StatementNodeArray);
Var
	I:Integer;
Begin
	Self.Statements := Statements;
	If Assigned(Statements) Then
		For I:=0 To Pred(Length(Statements)) Do
			Statements[I].SetParent(Self);
End;

Constructor ProgramNode.Create(Identifier:IdentifierNode; UsesDecl:UsesDeclNode; Declarationss:DeclarationsNodeArray; Block:BlockNode);
Var
	I:Integer;
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.UsesDecl := UsesDecl;
	If Assigned(UsesDecl) Then
		UsesDecl.SetParent(Self);
	Self.Declarationss := Declarationss;
	If Assigned(Declarationss) Then
		For I:=0 To Pred(Length(Declarationss)) Do
			Declarationss[I].SetParent(Self);
	Self.Block := Block;
	If Assigned(Block) Then
		Block.SetParent(Self);
End;

Constructor ConstDeclarationNode.Create(Identifier:IdentifierNode; Literal:LiteralNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.Literal := Literal;
	If Assigned(Literal) Then
		Literal.SetParent(Self);
End;

Constructor ConstsNode.Create(ConstDeclarations:ConstDeclarationNodeArray);
Var
	I:Integer;
Begin
	Self.ConstDeclarations := ConstDeclarations;
	If Assigned(ConstDeclarations) Then
		For I:=0 To Pred(Length(ConstDeclarations)) Do
			ConstDeclarations[I].SetParent(Self);
End;

Constructor VarDeclarationNode.Create(IdentifierList:IdentifierListNode; TypeID:IdentifierNode);
Begin
	Self.IdentifierList := IdentifierList;
	If Assigned(IdentifierList) Then
		IdentifierList.SetParent(Self);
	Self.TypeID := TypeID;
	If Assigned(TypeID) Then
		TypeID.SetParent(Self);
End;

Constructor VarsNode.Create(VarDeclarations:VarDeclarationNodeArray);
Var
	I:Integer;
Begin
	Self.VarDeclarations := VarDeclarations;
	If Assigned(VarDeclarations) Then
		For I:=0 To Pred(Length(VarDeclarations)) Do
			VarDeclarations[I].SetParent(Self);
End;

Constructor TypesNode.Create(TypeDeclarations:TypeDeclarationNodeArray);
Var
	I:Integer;
Begin
	Self.TypeDeclarations := TypeDeclarations;
	If Assigned(TypeDeclarations) Then
		For I:=0 To Pred(Length(TypeDeclarations)) Do
			TypeDeclarations[I].SetParent(Self);
End;

Constructor TypeAliasNode.Create(Identifier:IdentifierNode; Alias:IdentifierNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.Alias := Alias;
	If Assigned(Alias) Then
		Alias.SetParent(Self);
End;

Constructor RecordTypeNode.Create(Identifier:IdentifierNode; Const IsPacked:Boolean; VarDeclarations:VarDeclarationNodeArray);
Var
	I:Integer;
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.VarDeclarations := VarDeclarations;
	If Assigned(VarDeclarations) Then
		For I:=0 To Pred(Length(VarDeclarations)) Do
			VarDeclarations[I].SetParent(Self);
End;

Constructor EnumNode.Create(Identifier:IdentifierNode; IntegerLiteral:IntegerLiteralNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.IntegerLiteral := IntegerLiteral;
	If Assigned(IntegerLiteral) Then
		IntegerLiteral.SetParent(Self);
End;

Constructor EnumListNode.Create(Enums:EnumNodeArray);
Var
	I:Integer;
Begin
	Self.Enums := Enums;
	If Assigned(Enums) Then
		For I:=0 To Pred(Length(Enums)) Do
			Enums[I].SetParent(Self);
End;

Constructor EnumerationNode.Create(Identifier:IdentifierNode; EnumList:EnumListNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.EnumList := EnumList;
	If Assigned(EnumList) Then
		EnumList.SetParent(Self);
End;

Constructor RangeNode.Create(RangeStart:LiteralNode; RangeEnd:LiteralNode);
Begin
	Self.RangeStart := RangeStart;
	If Assigned(RangeStart) Then
		RangeStart.SetParent(Self);
	Self.RangeEnd := RangeEnd;
	If Assigned(RangeEnd) Then
		RangeEnd.SetParent(Self);
End;

Constructor RangeTypeNode.Create(Identifier:IdentifierNode; Range:RangeNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.Range := Range;
	If Assigned(Range) Then
		Range.SetParent(Self);
End;

Constructor RangeListNode.Create(Ranges:RangeNodeArray);
Var
	I:Integer;
Begin
	Self.Ranges := Ranges;
	If Assigned(Ranges) Then
		For I:=0 To Pred(Length(Ranges)) Do
			Ranges[I].SetParent(Self);
End;

Constructor ArrayNode.Create(RangeList:RangeListNode; Identifier:IdentifierNode);
Begin
	Self.RangeList := RangeList;
	If Assigned(RangeList) Then
		RangeList.SetParent(Self);
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
End;

Constructor ArrayTypeNode.Create(Identifier:IdentifierNode; ArrayValue:ArrayNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.ArrayValue := ArrayValue;
	If Assigned(ArrayValue) Then
		ArrayValue.SetParent(Self);
End;

Constructor RoutineArgumentDeclarationNode.Create(IdentifierList:IdentifierListNode; TypeID:IdentifierNode);
Begin
	Self.IdentifierList := IdentifierList;
	If Assigned(IdentifierList) Then
		IdentifierList.SetParent(Self);
	Self.TypeID := TypeID;
	If Assigned(TypeID) Then
		TypeID.SetParent(Self);
End;

Constructor RoutineArgumentsNode.Create(RoutineArgumentDeclarations:RoutineArgumentDeclarationNodeArray);
Var
	I:Integer;
Begin
	Self.RoutineArgumentDeclarations := RoutineArgumentDeclarations;
	If Assigned(RoutineArgumentDeclarations) Then
		For I:=0 To Pred(Length(RoutineArgumentDeclarations)) Do
			RoutineArgumentDeclarations[I].SetParent(Self);
End;

Constructor RoutineReturnTypeNode.Create(Identifier:IdentifierNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
End;

Constructor FunctionsNode.Create(RoutineDeclaration:RoutineDeclarationNode; RoutineArguments:RoutineArgumentsNode; RoutineReturnType:RoutineReturnTypeNode; Declarationss:DeclarationsNodeArray; Block:BlockNode);
Var
	I:Integer;
Begin
	Self.RoutineDeclaration := RoutineDeclaration;
	If Assigned(RoutineDeclaration) Then
		RoutineDeclaration.SetParent(Self);
	Self.RoutineArguments := RoutineArguments;
	If Assigned(RoutineArguments) Then
		RoutineArguments.SetParent(Self);
	Self.RoutineReturnType := RoutineReturnType;
	If Assigned(RoutineReturnType) Then
		RoutineReturnType.SetParent(Self);
	Self.Declarationss := Declarationss;
	If Assigned(Declarationss) Then
		For I:=0 To Pred(Length(Declarationss)) Do
			Declarationss[I].SetParent(Self);
	Self.Block := Block;
	If Assigned(Block) Then
		Block.SetParent(Self);
End;

Constructor ProcedureDeclarationNode.Create(Identifier:IdentifierNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
End;

Constructor FunctionDeclarationNode.Create(Identifier:IdentifierNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
End;

Constructor AssignmentNode.Create(Identifier:IdentifierNode; Expression:ExpressionNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.Expression := Expression;
	If Assigned(Expression) Then
		Expression.SetParent(Self);
End;

Constructor WhileStatementNode.Create(Condition:ExpressionNode; Statements:StatementNodeArray);
Var
	I:Integer;
Begin
	Self.Condition := Condition;
	If Assigned(Condition) Then
		Condition.SetParent(Self);
	Self.Statements := Statements;
	If Assigned(Statements) Then
		For I:=0 To Pred(Length(Statements)) Do
			Statements[I].SetParent(Self);
End;

Constructor RepeatStatementNode.Create(Statements:StatementNodeArray; Condition:ExpressionNode);
Var
	I:Integer;
Begin
	Self.Statements := Statements;
	If Assigned(Statements) Then
		For I:=0 To Pred(Length(Statements)) Do
			Statements[I].SetParent(Self);
	Self.Condition := Condition;
	If Assigned(Condition) Then
		Condition.SetParent(Self);
End;

Constructor ForStatementNode.Create(Assignment:AssignmentNode; Expression:ExpressionNode; Statements:StatementNodeArray);
Var
	I:Integer;
Begin
	Self.Assignment := Assignment;
	If Assigned(Assignment) Then
		Assignment.SetParent(Self);
	Self.Expression := Expression;
	If Assigned(Expression) Then
		Expression.SetParent(Self);
	Self.Statements := Statements;
	If Assigned(Statements) Then
		For I:=0 To Pred(Length(Statements)) Do
			Statements[I].SetParent(Self);
End;

Constructor IfStatementNode.Create(Expression:ExpressionNode; TrueStatement:StatementNode; FalseStatement:StatementNode);
Begin
	Self.Expression := Expression;
	If Assigned(Expression) Then
		Expression.SetParent(Self);
	Self.TrueStatement := TrueStatement;
	If Assigned(TrueStatement) Then
		TrueStatement.SetParent(Self);
	Self.FalseStatement := FalseStatement;
	If Assigned(FalseStatement) Then
		FalseStatement.SetParent(Self);
End;

Constructor ExpressionListNode.Create(Expressions:ExpressionNodeArray);
Var
	I:Integer;
Begin
	Self.Expressions := Expressions;
	If Assigned(Expressions) Then
		For I:=0 To Pred(Length(Expressions)) Do
			Expressions[I].SetParent(Self);
End;

Constructor FunctionCallNode.Create(Identifier:IdentifierNode; ExpressionList:ExpressionListNode);
Begin
	Self.Identifier := Identifier;
	If Assigned(Identifier) Then
		Identifier.SetParent(Self);
	Self.ExpressionList := ExpressionList;
	If Assigned(ExpressionList) Then
		ExpressionList.SetParent(Self);
End;

Constructor PriorityExpressionNode.Create(Expression:ExpressionNode);
Begin
	Self.Expression := Expression;
	If Assigned(Expression) Then
		Expression.SetParent(Self);
End;

Constructor UnaryExpressionNode.Create(UnaryOperator:UnaryOperatorNode; Expression:ExpressionNode);
Begin
	Self.UnaryOperator := UnaryOperator;
	If Assigned(UnaryOperator) Then
		UnaryOperator.SetParent(Self);
	Self.Expression := Expression;
	If Assigned(Expression) Then
		Expression.SetParent(Self);
End;

Constructor UnaryOperatorPlusNode.Create();
Begin
End;

Constructor UnaryOperatorMinusNode.Create();
Begin
End;

Constructor UnaryOperatorAddressOfNode.Create();
Begin
End;

Constructor UnaryOperatorNotNode.Create();
Begin
End;

Constructor BinaryExpressionNode.Create(ExprA:ExpressionNode; BinaryOperator:BinaryOperatorNode; ExprB:ExpressionNode);
Begin
	Self.ExprA := ExprA;
	If Assigned(ExprA) Then
		ExprA.SetParent(Self);
	Self.BinaryOperator := BinaryOperator;
	If Assigned(BinaryOperator) Then
		BinaryOperator.SetParent(Self);
	Self.ExprB := ExprB;
	If Assigned(ExprB) Then
		ExprB.SetParent(Self);
End;

Constructor LogicBinaryExpressionNode.Create(ExprA:ExpressionNode; LogicBinaryOperator:LogicBinaryOperatorNode; ExprB:ExpressionNode);
Begin
	Self.ExprA := ExprA;
	If Assigned(ExprA) Then
		ExprA.SetParent(Self);
	Self.LogicBinaryOperator := LogicBinaryOperator;
	If Assigned(LogicBinaryOperator) Then
		LogicBinaryOperator.SetParent(Self);
	Self.ExprB := ExprB;
	If Assigned(ExprB) Then
		ExprB.SetParent(Self);
End;

Constructor LogicBinaryOperatorEqualityNode.Create();
Begin
End;

Constructor LogicBinaryOperatorInequalityNode.Create();
Begin
End;

Constructor LogicBinaryOperatorAndNode.Create();
Begin
End;

Constructor LogicBinaryOperatorOrNode.Create();
Begin
End;

Constructor LogicBinaryOperatorXorNode.Create();
Begin
End;

Constructor LogicBinaryOperatorGreaterNode.Create();
Begin
End;

Constructor LogicBinaryOperatorGreaterOrEqualNode.Create();
Begin
End;

Constructor LogicBinaryOperatorLessNode.Create();
Begin
End;

Constructor LogicBinaryOperatorLessOrEqualNode.Create();
Begin
End;

Constructor LogicBinaryOperatorInsideNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorPlusNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorMinusNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorDivisionNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorModulusNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorProductNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorShiftLeftNode.Create();
Begin
End;

Constructor ArithmeticBinaryOperatorShiftRightNode.Create();
Begin
End;

Constructor DelphiLexer.Create();
Begin
	AddMatcher(StringTokenMatcher.Create(1, 'true', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(1, 'false', lexerAccept));
	AddMatcher(IntegerLiteral1RegexMatcher.Create(2, 1, lexerAccept));  // [+-]?[0-9]+
	AddMatcher(IntegerLiteral2RegexMatcher.Create(2, 0, lexerAccept));  // [+-]?[$][0-9a-f]*
	AddMatcher(FloatLiteral1RegexMatcher.Create(3, 1, lexerAccept));  // [+-]?[0-9]+.[0-9]+
	AddMatcher(FloatLiteral2RegexMatcher.Create(3, 1, lexerAccept));  // [+-]?[0-9]+e[+-]?[0-9]+
	AddMatcher(FloatLiteral3RegexMatcher.Create(3, 1, lexerAccept));  // [+-]?[0-9]+.[0-9]+e[+-]?[0-9]+
	AddMatcher(StringLiteralRegexMatcher.Create(4, 0, lexerAccept));  // '[^']*'
	AddMatcher(CharLiteralRegexMatcher.Create(5, 1, lexerAccept));  // [#][0-9]+
	AddMatcher(IdentifierRegexMatcher.Create(6, 0, lexerAccept));  // [_a-zA-Z][0-9a-zA-Z_]*
	AddMatcher(CharTokenMatcher.Create(7, 44, lexerAccept));  // ,
	AddMatcher(WhiteSpaceRegexMatcher.Create(8, 0, lexerDiscard));  // [ \n	]*
	AddMatcher(StringTokenMatcher.Create(9, 'program', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(10, 59, lexerAccept));  // ;
	AddMatcher(CharTokenMatcher.Create(11, 46, lexerAccept));  // .
	AddMatcher(StringTokenMatcher.Create(12, 'uses', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(13, 'const', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(14, 61, lexerAccept));  // =
	AddMatcher(StringTokenMatcher.Create(15, 'var', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(16, 58, lexerAccept));  // :
	AddMatcher(StringTokenMatcher.Create(17, 'type', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(18, 'packed', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(19, 'record', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(20, 'end', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(21, 40, lexerAccept));  // (
	AddMatcher(CharTokenMatcher.Create(22, 41, lexerAccept));  // )
	AddMatcher(StringTokenMatcher.Create(23, '..', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(24, 'array', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(25, 91, lexerAccept));  // [
	AddMatcher(CharTokenMatcher.Create(26, 93, lexerAccept));  // ]
	AddMatcher(StringTokenMatcher.Create(27, 'of', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(28, 'procedure', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(29, 'function', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(30, 'begin', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(31, ':=', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(32, 'while', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(33, 'do', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(34, 'repeat', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(35, 'until', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(36, 'for', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(37, 'to', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(38, 'if', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(39, 'then', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(40, 'else', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(41, 43, lexerAccept));  // +
	AddMatcher(CharTokenMatcher.Create(42, 45, lexerAccept));  // -
	AddMatcher(CharTokenMatcher.Create(43, 64, lexerAccept));  // @
	AddMatcher(StringTokenMatcher.Create(44, 'not', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(45, '<>', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(46, 'and', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(47, 'or', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(48, 'xor', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(49, 62, lexerAccept));  // >
	AddMatcher(StringTokenMatcher.Create(50, '>=', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(51, 60, lexerAccept));  // <
	AddMatcher(StringTokenMatcher.Create(52, '<=', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(53, 'in', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(54, 'div', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(55, 'mod', lexerAccept));
	AddMatcher(CharTokenMatcher.Create(56, 42, lexerAccept));  // *
	AddMatcher(StringTokenMatcher.Create(57, 'shr', lexerAccept));
	AddMatcher(StringTokenMatcher.Create(58, 'shl', lexerAccept));
End;

Procedure DelphiProcessor.VisitGoal(Node:GoalNode);
Begin
	If (Node Is ProgramNode) Then
		VisitProgram(ProgramNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitExpression(Node:ExpressionNode);
Begin
	If (Node Is LiteralNode) Then
		VisitLiteral(LiteralNode(Node))
	Else
	If (Node Is IdentifierNode) Then
		VisitIdentifier(IdentifierNode(Node))
	Else
	If (Node Is RangeNode) Then
		VisitRange(RangeNode(Node))
	Else
	If (Node Is FunctionCallNode) Then
		VisitFunctionCall(FunctionCallNode(Node))
	Else
	If (Node Is PriorityExpressionNode) Then
		VisitPriorityExpression(PriorityExpressionNode(Node))
	Else
	If (Node Is UnaryExpressionNode) Then
		VisitUnaryExpression(UnaryExpressionNode(Node))
	Else
	If (Node Is BinaryExpressionNode) Then
		VisitBinaryExpression(BinaryExpressionNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitLiteral(Node:LiteralNode);
Begin
	If (Node Is BoolLiteralNode) Then
		VisitBoolLiteral(BoolLiteralNode(Node))
	Else
	If (Node Is IntegerLiteralNode) Then
		VisitIntegerLiteral(IntegerLiteralNode(Node))
	Else
	If (Node Is FloatLiteralNode) Then
		VisitFloatLiteral(FloatLiteralNode(Node))
	Else
	If (Node Is StringLiteralNode) Then
		VisitStringLiteral(StringLiteralNode(Node))
	Else
	If (Node Is CharLiteralNode) Then
		VisitCharLiteral(CharLiteralNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitBoolLiteral(Node:BoolLiteralNode);
Begin
End;

Procedure DelphiProcessor.VisitIntegerLiteral(Node:IntegerLiteralNode);
Begin
End;

Procedure DelphiProcessor.VisitFloatLiteral(Node:FloatLiteralNode);
Begin
End;

Procedure DelphiProcessor.VisitStringLiteral(Node:StringLiteralNode);
Begin
End;

Procedure DelphiProcessor.VisitCharLiteral(Node:CharLiteralNode);
Begin
End;

Procedure DelphiProcessor.VisitIdentifier(Node:IdentifierNode);
Begin
End;

Procedure DelphiProcessor.VisitIdentifierList(Node:IdentifierListNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Identifiers)) Do
		VisitIdentifier(Node.Identifiers[I]);
End;

Procedure DelphiProcessor.VisitWhiteSpace(Node:WhiteSpaceNode);
Begin
End;

Procedure DelphiProcessor.VisitUsesDecl(Node:UsesDeclNode);
Begin
	VisitIdentifierList(Node.IdentifierList);
End;

Procedure DelphiProcessor.VisitDeclarations(Node:DeclarationsNode);
Begin
	If (Node Is ConstsNode) Then
		VisitConsts(ConstsNode(Node))
	Else
	If (Node Is VarsNode) Then
		VisitVars(VarsNode(Node))
	Else
	If (Node Is TypesNode) Then
		VisitTypes(TypesNode(Node))
	Else
	If (Node Is FunctionsNode) Then
		VisitFunctions(FunctionsNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitStatement(Node:StatementNode);
Begin
	If (Node Is BlockNode) Then
		VisitBlock(BlockNode(Node))
	Else
	If (Node Is AssignmentNode) Then
		VisitAssignment(AssignmentNode(Node))
	Else
	If (Node Is WhileStatementNode) Then
		VisitWhileStatement(WhileStatementNode(Node))
	Else
	If (Node Is RepeatStatementNode) Then
		VisitRepeatStatement(RepeatStatementNode(Node))
	Else
	If (Node Is ForStatementNode) Then
		VisitForStatement(ForStatementNode(Node))
	Else
	If (Node Is IfStatementNode) Then
		VisitIfStatement(IfStatementNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitBlock(Node:BlockNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Statements)) Do
		VisitStatement(Node.Statements[I]);
End;

Procedure DelphiProcessor.VisitProgram(Node:ProgramNode);
Var
	I:Integer;
Begin
	VisitIdentifier(Node.Identifier);
	VisitUsesDecl(Node.UsesDecl);
	For I:=0 To Pred(Length(Node.Declarationss)) Do
		VisitDeclarations(Node.Declarationss[I]);
	VisitBlock(Node.Block);
End;

Procedure DelphiProcessor.VisitConstDeclaration(Node:ConstDeclarationNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitLiteral(Node.Literal);
End;

Procedure DelphiProcessor.VisitConsts(Node:ConstsNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.ConstDeclarations)) Do
		VisitConstDeclaration(Node.ConstDeclarations[I]);
End;

Procedure DelphiProcessor.VisitVarDeclaration(Node:VarDeclarationNode);
Begin
	VisitIdentifierList(Node.IdentifierList);
	VisitIdentifier(Node.TypeID);
End;

Procedure DelphiProcessor.VisitVars(Node:VarsNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.VarDeclarations)) Do
		VisitVarDeclaration(Node.VarDeclarations[I]);
End;

Procedure DelphiProcessor.VisitTypeDeclaration(Node:TypeDeclarationNode);
Begin
	If (Node Is TypeAliasNode) Then
		VisitTypeAlias(TypeAliasNode(Node))
	Else
	If (Node Is RecordTypeNode) Then
		VisitRecordType(RecordTypeNode(Node))
	Else
	If (Node Is EnumerationNode) Then
		VisitEnumeration(EnumerationNode(Node))
	Else
	If (Node Is RangeTypeNode) Then
		VisitRangeType(RangeTypeNode(Node))
	Else
	If (Node Is ArrayTypeNode) Then
		VisitArrayType(ArrayTypeNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitTypes(Node:TypesNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.TypeDeclarations)) Do
		VisitTypeDeclaration(Node.TypeDeclarations[I]);
End;

Procedure DelphiProcessor.VisitTypeAlias(Node:TypeAliasNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitIdentifier(Node.Alias);
End;

Procedure DelphiProcessor.VisitRecordType(Node:RecordTypeNode);
Var
	I:Integer;
Begin
	VisitIdentifier(Node.Identifier);
	For I:=0 To Pred(Length(Node.VarDeclarations)) Do
		VisitVarDeclaration(Node.VarDeclarations[I]);
End;

Procedure DelphiProcessor.VisitEnum(Node:EnumNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitIntegerLiteral(Node.IntegerLiteral);
End;

Procedure DelphiProcessor.VisitEnumList(Node:EnumListNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Enums)) Do
		VisitEnum(Node.Enums[I]);
End;

Procedure DelphiProcessor.VisitEnumeration(Node:EnumerationNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitEnumList(Node.EnumList);
End;

Procedure DelphiProcessor.VisitRange(Node:RangeNode);
Begin
	VisitLiteral(Node.RangeStart);
	VisitLiteral(Node.RangeEnd);
End;

Procedure DelphiProcessor.VisitRangeType(Node:RangeTypeNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitRange(Node.Range);
End;

Procedure DelphiProcessor.VisitRangeList(Node:RangeListNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Ranges)) Do
		VisitRange(Node.Ranges[I]);
End;

Procedure DelphiProcessor.VisitArray(Node:ArrayNode);
Begin
	VisitRangeList(Node.RangeList);
	VisitIdentifier(Node.Identifier);
End;

Procedure DelphiProcessor.VisitArrayType(Node:ArrayTypeNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitArray(Node.ArrayValue);
End;

Procedure DelphiProcessor.VisitRoutineDeclaration(Node:RoutineDeclarationNode);
Begin
	If (Node Is ProcedureDeclarationNode) Then
		VisitProcedureDeclaration(ProcedureDeclarationNode(Node))
	Else
	If (Node Is FunctionDeclarationNode) Then
		VisitFunctionDeclaration(FunctionDeclarationNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitRoutineArgumentDeclaration(Node:RoutineArgumentDeclarationNode);
Begin
	VisitIdentifierList(Node.IdentifierList);
	VisitIdentifier(Node.TypeID);
End;

Procedure DelphiProcessor.VisitRoutineArguments(Node:RoutineArgumentsNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.RoutineArgumentDeclarations)) Do
		VisitRoutineArgumentDeclaration(Node.RoutineArgumentDeclarations[I]);
End;

Procedure DelphiProcessor.VisitRoutineReturnType(Node:RoutineReturnTypeNode);
Begin
	VisitIdentifier(Node.Identifier);
End;

Procedure DelphiProcessor.VisitFunctions(Node:FunctionsNode);
Var
	I:Integer;
Begin
	VisitRoutineDeclaration(Node.RoutineDeclaration);
	VisitRoutineArguments(Node.RoutineArguments);
	VisitRoutineReturnType(Node.RoutineReturnType);
	For I:=0 To Pred(Length(Node.Declarationss)) Do
		VisitDeclarations(Node.Declarationss[I]);
	VisitBlock(Node.Block);
End;

Procedure DelphiProcessor.VisitProcedureDeclaration(Node:ProcedureDeclarationNode);
Begin
	VisitIdentifier(Node.Identifier);
End;

Procedure DelphiProcessor.VisitFunctionDeclaration(Node:FunctionDeclarationNode);
Begin
	VisitIdentifier(Node.Identifier);
End;

Procedure DelphiProcessor.VisitAssignment(Node:AssignmentNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitExpression(Node.Expression);
End;

Procedure DelphiProcessor.VisitWhileStatement(Node:WhileStatementNode);
Var
	I:Integer;
Begin
	VisitExpression(Node.Condition);
	For I:=0 To Pred(Length(Node.Statements)) Do
		VisitStatement(Node.Statements[I]);
End;

Procedure DelphiProcessor.VisitRepeatStatement(Node:RepeatStatementNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Statements)) Do
		VisitStatement(Node.Statements[I]);
	VisitExpression(Node.Condition);
End;

Procedure DelphiProcessor.VisitForStatement(Node:ForStatementNode);
Var
	I:Integer;
Begin
	VisitAssignment(Node.Assignment);
	VisitExpression(Node.Expression);
	For I:=0 To Pred(Length(Node.Statements)) Do
		VisitStatement(Node.Statements[I]);
End;

Procedure DelphiProcessor.VisitIfStatement(Node:IfStatementNode);
Begin
	VisitExpression(Node.Expression);
	VisitStatement(Node.TrueStatement);
	VisitStatement(Node.FalseStatement);
End;

Procedure DelphiProcessor.VisitExpressionList(Node:ExpressionListNode);
Var
	I:Integer;
Begin
	For I:=0 To Pred(Length(Node.Expressions)) Do
		VisitExpression(Node.Expressions[I]);
End;

Procedure DelphiProcessor.VisitFunctionCall(Node:FunctionCallNode);
Begin
	VisitIdentifier(Node.Identifier);
	VisitExpressionList(Node.ExpressionList);
End;

Procedure DelphiProcessor.VisitPriorityExpression(Node:PriorityExpressionNode);
Begin
	VisitExpression(Node.Expression);
End;

Procedure DelphiProcessor.VisitUnaryOperator(Node:UnaryOperatorNode);
Begin
	If (Node Is UnaryOperatorPlusNode) Then
		VisitUnaryOperatorPlus(UnaryOperatorPlusNode(Node))
	Else
	If (Node Is UnaryOperatorMinusNode) Then
		VisitUnaryOperatorMinus(UnaryOperatorMinusNode(Node))
	Else
	If (Node Is UnaryOperatorAddressOfNode) Then
		VisitUnaryOperatorAddressOf(UnaryOperatorAddressOfNode(Node))
	Else
	If (Node Is UnaryOperatorNotNode) Then
		VisitUnaryOperatorNot(UnaryOperatorNotNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitUnaryExpression(Node:UnaryExpressionNode);
Begin
	VisitUnaryOperator(Node.UnaryOperator);
	VisitExpression(Node.Expression);
End;

Procedure DelphiProcessor.VisitUnaryOperatorPlus(Node:UnaryOperatorPlusNode);
Begin
End;

Procedure DelphiProcessor.VisitUnaryOperatorMinus(Node:UnaryOperatorMinusNode);
Begin
End;

Procedure DelphiProcessor.VisitUnaryOperatorAddressOf(Node:UnaryOperatorAddressOfNode);
Begin
End;

Procedure DelphiProcessor.VisitUnaryOperatorNot(Node:UnaryOperatorNotNode);
Begin
End;

Procedure DelphiProcessor.VisitBinaryOperator(Node:BinaryOperatorNode);
Begin
	If (Node Is LogicBinaryOperatorNode) Then
		VisitLogicBinaryOperator(LogicBinaryOperatorNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorNode) Then
		VisitArithmeticBinaryOperator(ArithmeticBinaryOperatorNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitBinaryExpression(Node:BinaryExpressionNode);
Begin
	VisitExpression(Node.ExprA);
	VisitBinaryOperator(Node.BinaryOperator);
	VisitExpression(Node.ExprB);
End;

Procedure DelphiProcessor.VisitLogicBinaryOperator(Node:LogicBinaryOperatorNode);
Begin
	If (Node Is LogicBinaryOperatorEqualityNode) Then
		VisitLogicBinaryOperatorEquality(LogicBinaryOperatorEqualityNode(Node))
	Else
	If (Node Is LogicBinaryOperatorInequalityNode) Then
		VisitLogicBinaryOperatorInequality(LogicBinaryOperatorInequalityNode(Node))
	Else
	If (Node Is LogicBinaryOperatorAndNode) Then
		VisitLogicBinaryOperatorAnd(LogicBinaryOperatorAndNode(Node))
	Else
	If (Node Is LogicBinaryOperatorOrNode) Then
		VisitLogicBinaryOperatorOr(LogicBinaryOperatorOrNode(Node))
	Else
	If (Node Is LogicBinaryOperatorXorNode) Then
		VisitLogicBinaryOperatorXor(LogicBinaryOperatorXorNode(Node))
	Else
	If (Node Is LogicBinaryOperatorGreaterNode) Then
		VisitLogicBinaryOperatorGreater(LogicBinaryOperatorGreaterNode(Node))
	Else
	If (Node Is LogicBinaryOperatorGreaterOrEqualNode) Then
		VisitLogicBinaryOperatorGreaterOrEqual(LogicBinaryOperatorGreaterOrEqualNode(Node))
	Else
	If (Node Is LogicBinaryOperatorLessNode) Then
		VisitLogicBinaryOperatorLess(LogicBinaryOperatorLessNode(Node))
	Else
	If (Node Is LogicBinaryOperatorLessOrEqualNode) Then
		VisitLogicBinaryOperatorLessOrEqual(LogicBinaryOperatorLessOrEqualNode(Node))
	Else
	If (Node Is LogicBinaryOperatorInsideNode) Then
		VisitLogicBinaryOperatorInside(LogicBinaryOperatorInsideNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitLogicBinaryExpression(Node:LogicBinaryExpressionNode);
Begin
	VisitExpression(Node.ExprA);
	VisitLogicBinaryOperator(Node.LogicBinaryOperator);
	VisitExpression(Node.ExprB);
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorEquality(Node:LogicBinaryOperatorEqualityNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorInequality(Node:LogicBinaryOperatorInequalityNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorAnd(Node:LogicBinaryOperatorAndNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorOr(Node:LogicBinaryOperatorOrNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorXor(Node:LogicBinaryOperatorXorNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorGreater(Node:LogicBinaryOperatorGreaterNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorGreaterOrEqual(Node:LogicBinaryOperatorGreaterOrEqualNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorLess(Node:LogicBinaryOperatorLessNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorLessOrEqual(Node:LogicBinaryOperatorLessOrEqualNode);
Begin
End;

Procedure DelphiProcessor.VisitLogicBinaryOperatorInside(Node:LogicBinaryOperatorInsideNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperator(Node:ArithmeticBinaryOperatorNode);
Begin
	If (Node Is ArithmeticBinaryOperatorPlusNode) Then
		VisitArithmeticBinaryOperatorPlus(ArithmeticBinaryOperatorPlusNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorMinusNode) Then
		VisitArithmeticBinaryOperatorMinus(ArithmeticBinaryOperatorMinusNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorDivisionNode) Then
		VisitArithmeticBinaryOperatorDivision(ArithmeticBinaryOperatorDivisionNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorModulusNode) Then
		VisitArithmeticBinaryOperatorModulus(ArithmeticBinaryOperatorModulusNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorProductNode) Then
		VisitArithmeticBinaryOperatorProduct(ArithmeticBinaryOperatorProductNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorShiftLeftNode) Then
		VisitArithmeticBinaryOperatorShiftLeft(ArithmeticBinaryOperatorShiftLeftNode(Node))
	Else
	If (Node Is ArithmeticBinaryOperatorShiftRightNode) Then
		VisitArithmeticBinaryOperatorShiftRight(ArithmeticBinaryOperatorShiftRightNode(Node))
	Else
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorPlus(Node:ArithmeticBinaryOperatorPlusNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorMinus(Node:ArithmeticBinaryOperatorMinusNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorDivision(Node:ArithmeticBinaryOperatorDivisionNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorModulus(Node:ArithmeticBinaryOperatorModulusNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorProduct(Node:ArithmeticBinaryOperatorProductNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorShiftLeft(Node:ArithmeticBinaryOperatorShiftLeftNode);
Begin
End;

Procedure DelphiProcessor.VisitArithmeticBinaryOperatorShiftRight(Node:ArithmeticBinaryOperatorShiftRightNode);
Begin
End;


Function DelphiParser.Parse(Source:Stream; IgnoreCase:Boolean):ASTNode;
Begin
	_Lexer := DelphiLexer.Create();
	Result := Inherited Parse(Source, IgnoreCase);
End;

Function DelphiParser.GetTokenName(ID:Cardinal; Const Value:TERRAString):TERRAString;
Begin
	Case ID Of
		1: Result := 'BoolLiteral';
		2: Result := 'IntegerLiteral';
		3: Result := 'FloatLiteral';
		4: Result := 'StringLiteral';
		5: Result := 'CharLiteral';
		6: Result := 'Identifier';
		7: Result := ',';
		8: Result := 'WhiteSpace';
		9: Result := 'program';
		10: Result := ';';
		11: Result := '.';
		12: Result := 'uses';
		13: Result := 'const';
		14: Result := '=';
		15: Result := 'var';
		16: Result := ':';
		17: Result := 'type';
		18: Result := 'packed';
		19: Result := 'record';
		20: Result := 'end';
		21: Result := '(';
		22: Result := ')';
		23: Result := '..';
		24: Result := 'array';
		25: Result := '[';
		26: Result := ']';
		27: Result := 'of';
		28: Result := 'procedure';
		29: Result := 'function';
		30: Result := 'begin';
		31: Result := ':=';
		32: Result := 'while';
		33: Result := 'do';
		34: Result := 'repeat';
		35: Result := 'until';
		36: Result := 'for';
		37: Result := 'to';
		38: Result := 'if';
		39: Result := 'then';
		40: Result := 'else';
		41: Result := '+';
		42: Result := '-';
		43: Result := '@';
		44: Result := 'not';
		45: Result := '<>';
		46: Result := 'and';
		47: Result := 'or';
		48: Result := 'xor';
		49: Result := '>';
		50: Result := '>=';
		51: Result := '<';
		52: Result := '<=';
		53: Result := 'in';
		54: Result := 'div';
		55: Result := 'mod';
		56: Result := '*';
		57: Result := 'shr';
		58: Result := 'shl';
	Else
		Result := CardinalToString(ID);
	End;
	Result := '['+Result+']';
End;

Function DelphiParser.ParseGoal():ASTNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseProgram(GoalNode, False);
	If (Result = Nil) Then
		ParsingExceptedError(GoalNode, '');
End;

Function DelphiParser.ParseExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):ExpressionNode;
Var
	TempToken:LexerToken;
	LookAheadToken:LexerToken;
	BinaryOperator:BinaryOperatorNode;
	ExprB:ExpressionNode;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParsePriorityExpression(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		LookAheadToken := Self.CurrentToken;
		BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
		If BinaryOperator = Nil Then
		Begin
			Self.CurrentToken := LookAheadToken;
			Exit;
		End;
		ExprB := Self.ParseExpression(HandlerNode, True);
		If Assigned(ExprB) Then
		Begin
			Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
			Exit;
		End;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseUnaryExpression(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		LookAheadToken := Self.CurrentToken;
		BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
		If BinaryOperator = Nil Then
		Begin
			Self.CurrentToken := LookAheadToken;
			Exit;
		End;
		ExprB := Self.ParseExpression(HandlerNode, True);
		If Assigned(ExprB) Then
		Begin
			Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
			Exit;
		End;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseFunctionCall(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		LookAheadToken := Self.CurrentToken;
		BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
		If BinaryOperator = Nil Then
		Begin
			Self.CurrentToken := LookAheadToken;
			Exit;
		End;
		ExprB := Self.ParseExpression(HandlerNode, True);
		If Assigned(ExprB) Then
		Begin
			Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
			Exit;
		End;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseRange(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		LookAheadToken := Self.CurrentToken;
		BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
		If BinaryOperator = Nil Then
		Begin
			Self.CurrentToken := LookAheadToken;
			Exit;
		End;
		ExprB := Self.ParseExpression(HandlerNode, True);
		If Assigned(ExprB) Then
		Begin
			Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
			Exit;
		End;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLiteral(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		LookAheadToken := Self.CurrentToken;
		BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
		If BinaryOperator = Nil Then
		Begin
			Self.CurrentToken := LookAheadToken;
			Exit;
		End;
		ExprB := Self.ParseExpression(HandlerNode, True);
		If Assigned(ExprB) Then
		Begin
			Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
			Exit;
		End;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseIdentifier(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(ExpressionNode, '');
	LookAheadToken := Self.CurrentToken;
	BinaryOperator := Self.ParseBinaryOperator(HandlerNode, True);
	If BinaryOperator = Nil Then
	Begin
		Self.CurrentToken := LookAheadToken;
		Exit;
	End;
	ExprB := Self.ParseExpression(HandlerNode, True);
	If Assigned(ExprB) Then
	Begin
		Result := BinaryExpressionNode.Create(Result, BinaryOperator, ExprB);
		Exit;
	End;
End;

Function DelphiParser.ParseLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):LiteralNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseStringLiteral(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseCharLiteral(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseIntegerLiteral(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseFloatLiteral(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseBoolLiteral(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(LiteralNode, '');
End;

Function DelphiParser.ParseBoolLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):BoolLiteralNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 1)) Then
		Exit;
	Value := TokenValue();
	Result := BoolLiteralNode.Create(Value);
End;

Function DelphiParser.ParseIntegerLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):IntegerLiteralNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 2)) Then
		Exit;
	Value := TokenValue();
	Result := IntegerLiteralNode.Create(Value);
End;

Function DelphiParser.ParseFloatLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):FloatLiteralNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 3)) Then
		Exit;
	Value := TokenValue();
	Result := FloatLiteralNode.Create(Value);
End;

Function DelphiParser.ParseStringLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):StringLiteralNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 4)) Then
		Exit;
	Value := TokenValue();
	Result := StringLiteralNode.Create(Value);
End;

Function DelphiParser.ParseCharLiteral(HandlerNode:ASTNodeClass; IsOpt:Boolean):CharLiteralNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 5)) Then
		Exit;
	Value := TokenValue();
	Result := CharLiteralNode.Create(Value);
End;

Function DelphiParser.ParseIdentifier(HandlerNode:ASTNodeClass; IsOpt:Boolean):IdentifierNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 6)) Then
		Exit;
	Value := TokenValue();
	Result := IdentifierNode.Create(Value);
End;

Function DelphiParser.ParseIdentifierList(HandlerNode:ASTNodeClass; IsOpt:Boolean):IdentifierListNode;
Var
	Identifiers :IdentifierNodeArray;
	Identifier :IdentifierNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Identifier := Self.ParseIdentifier(HandlerNode, True);
	If Assigned(Identifier) Then
	Begin
		SetLength(Identifiers, Succ(Length(Identifiers)));
		Identifiers[Pred(Length(Identifiers))] := Identifier;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 7)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := IdentifierListNode.Create(Identifiers);
End;

Function DelphiParser.ParseWhiteSpace(HandlerNode:ASTNodeClass; IsOpt:Boolean):WhiteSpaceNode;
Var
	Value :TERRAString;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 8)) Then
		Exit;
	Value := TokenValue();
	Result := WhiteSpaceNode.Create(Value);
End;

Function DelphiParser.ParseUsesDecl(HandlerNode:ASTNodeClass; IsOpt:Boolean):UsesDeclNode;
Var
	IdentifierList :IdentifierListNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 12)) Then
			Exit;

	HandlerNode := UsesDeclNode;
	IdentifierList := Self.ParseIdentifierList(HandlerNode, False);
	If IdentifierList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := UsesDeclNode.Create(IdentifierList);
End;

Function DelphiParser.ParseDeclarations(HandlerNode:ASTNodeClass; IsOpt:Boolean):DeclarationsNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseConsts(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseVars(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseTypes(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseFunctions(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(DeclarationsNode, '');
End;

Function DelphiParser.ParseStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):StatementNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseIfStatement(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseWhileStatement(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseRepeatStatement(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseForStatement(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseAssignment(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseBlock(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(StatementNode, '');
End;

Function DelphiParser.ParseBlock(HandlerNode:ASTNodeClass; IsOpt:Boolean):BlockNode;
Var
	Statements :StatementNodeArray;
	Statement :StatementNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 30)) Then
			Exit;

	HandlerNode := BlockNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Statement := Self.ParseStatement(HandlerNode, True);
	If Assigned(Statement) Then
	Begin
		SetLength(Statements, Succ(Length(Statements)));
		Statements[Pred(Length(Statements))] := Statement;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 10)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	If (Not NextToken().ExpectToken(HandlerNode, False, 20)) Then
			Exit;

	Result := BlockNode.Create(Statements);
End;

Function DelphiParser.ParseProgram(HandlerNode:ASTNodeClass; IsOpt:Boolean):ProgramNode;
Var
	Identifier :IdentifierNode;
	UsesDecl :UsesDeclNode;
	Declarationss :DeclarationsNodeArray;
	Declarations :DeclarationsNode;
	Block :BlockNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 9)) Then
			Exit;

	HandlerNode := ProgramNode;
	Identifier := Self.ParseIdentifier(HandlerNode, False);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	TempToken := Self.CurrentToken;
	Repeat
	UsesDecl := Self.ParseUsesDecl(HandlerNode, True);
	If UsesDecl= Nil Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until True;

	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Declarations := Self.ParseDeclarations(HandlerNode, True);
	If Assigned(Declarations) Then
	Begin
		SetLength(Declarationss, Succ(Length(Declarationss)));
		Declarationss[Pred(Length(Declarationss))] := Declarations;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	Block := Self.ParseBlock(HandlerNode, False);
	If Block= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 11)) Then
			Exit;

	Result := ProgramNode.Create(Identifier, UsesDecl, Declarationss, Block);
End;

Function DelphiParser.ParseConstDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):ConstDeclarationNode;
Var
	Identifier :IdentifierNode;
	Literal :LiteralNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	HandlerNode := ConstDeclarationNode;
	Literal := Self.ParseLiteral(HandlerNode, False);
	If Literal= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := ConstDeclarationNode.Create(Identifier, Literal);
End;

Function DelphiParser.ParseConsts(HandlerNode:ASTNodeClass; IsOpt:Boolean):ConstsNode;
Var
	ConstDeclarations :ConstDeclarationNodeArray;
	ConstDeclaration :ConstDeclarationNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 13)) Then
			Exit;

	HandlerNode := ConstsNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	ConstDeclaration := Self.ParseConstDeclaration(HandlerNode, True);
	If Assigned(ConstDeclaration) Then
	Begin
		SetLength(ConstDeclarations, Succ(Length(ConstDeclarations)));
		ConstDeclarations[Pred(Length(ConstDeclarations))] := ConstDeclaration;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	Result := ConstsNode.Create(ConstDeclarations);
End;

Function DelphiParser.ParseVarDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):VarDeclarationNode;
Var
	IdentifierList :IdentifierListNode;
	TypeID :IdentifierNode;
Begin
	Result := Nil;
	IdentifierList := Self.ParseIdentifierList(HandlerNode, IsOpt);
	If IdentifierList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 16)) Then
			Exit;

	HandlerNode := VarDeclarationNode;
	TypeID := Self.ParseIdentifier(HandlerNode, False);
	If TypeID= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := VarDeclarationNode.Create(IdentifierList, TypeID);
End;

Function DelphiParser.ParseVars(HandlerNode:ASTNodeClass; IsOpt:Boolean):VarsNode;
Var
	VarDeclarations :VarDeclarationNodeArray;
	VarDeclaration :VarDeclarationNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 15)) Then
			Exit;

	HandlerNode := VarsNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	VarDeclaration := Self.ParseVarDeclaration(HandlerNode, True);
	If Assigned(VarDeclaration) Then
	Begin
		SetLength(VarDeclarations, Succ(Length(VarDeclarations)));
		VarDeclarations[Pred(Length(VarDeclarations))] := VarDeclaration;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	Result := VarsNode.Create(VarDeclarations);
End;

Function DelphiParser.ParseTypeDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypeDeclarationNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseRecordType(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseTypeAlias(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseEnumeration(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseRangeType(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArrayType(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(TypeDeclarationNode, '');
End;

Function DelphiParser.ParseTypes(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypesNode;
Var
	TypeDeclarations :TypeDeclarationNodeArray;
	TypeDeclaration :TypeDeclarationNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 17)) Then
			Exit;

	HandlerNode := TypesNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	TypeDeclaration := Self.ParseTypeDeclaration(HandlerNode, True);
	If Assigned(TypeDeclaration) Then
	Begin
		SetLength(TypeDeclarations, Succ(Length(TypeDeclarations)));
		TypeDeclarations[Pred(Length(TypeDeclarations))] := TypeDeclaration;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	Result := TypesNode.Create(TypeDeclarations);
End;

Function DelphiParser.ParseTypeAlias(HandlerNode:ASTNodeClass; IsOpt:Boolean):TypeAliasNode;
Var
	Identifier :IdentifierNode;
	Alias :IdentifierNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	Alias := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Alias= Nil Then
		Exit;

	HandlerNode := TypeAliasNode;
	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := TypeAliasNode.Create(Identifier, Alias);
End;

Function DelphiParser.ParseRecordType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RecordTypeNode;
Var
	Identifier :IdentifierNode;
	IsPacked :Boolean;
	VarDeclarations :VarDeclarationNodeArray;
	VarDeclaration :VarDeclarationNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	TempToken := Self.CurrentToken;
	Repeat
	IsPacked := False;
	If (Not NextToken().ExpectToken(HandlerNode, True, 18)) Then
		Begin
			VarDeclaration := Nil;
			Self.CurrentToken := TempToken;
			Break;
		End;
	IsPacked := True;

	Until True;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 19)) Then
			Exit;

	HandlerNode := RecordTypeNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	VarDeclaration := Self.ParseVarDeclaration(HandlerNode, True);
	If Assigned(VarDeclaration) Then
	Begin
		SetLength(VarDeclarations, Succ(Length(VarDeclarations)));
		VarDeclarations[Pred(Length(VarDeclarations))] := VarDeclaration;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	If (Not NextToken().ExpectToken(HandlerNode, False, 20)) Then
			Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := RecordTypeNode.Create(Identifier, IsPacked, VarDeclarations);
End;

Function DelphiParser.ParseEnum(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumNode;
Var
	Identifier :IdentifierNode;
	IntegerLiteral :IntegerLiteralNode;
	TempToken:LexerToken;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	TempToken := Self.CurrentToken;
	Repeat
	If (Not NextToken().ExpectToken(HandlerNode, True, 14)) Then
		Begin
			IntegerLiteral := Nil;
			Self.CurrentToken := TempToken;
			Break;
		End;

	HandlerNode := EnumNode;
	IntegerLiteral := Self.ParseIntegerLiteral(HandlerNode, True);
	If IntegerLiteral= Nil Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until True;

	Result := EnumNode.Create(Identifier, IntegerLiteral);
End;

Function DelphiParser.ParseEnumList(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumListNode;
Var
	Enums :EnumNodeArray;
	Enum :EnumNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Enum := Self.ParseEnum(HandlerNode, True);
	If Assigned(Enum) Then
	Begin
		SetLength(Enums, Succ(Length(Enums)));
		Enums[Pred(Length(Enums))] := Enum;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 7)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := EnumListNode.Create(Enums);
End;

Function DelphiParser.ParseEnumeration(HandlerNode:ASTNodeClass; IsOpt:Boolean):EnumerationNode;
Var
	Identifier :IdentifierNode;
	EnumList :EnumListNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 21)) Then
			Exit;

	HandlerNode := EnumerationNode;
	EnumList := Self.ParseEnumList(HandlerNode, False);
	If EnumList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 22)) Then
			Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := EnumerationNode.Create(Identifier, EnumList);
End;

Function DelphiParser.ParseRange(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeNode;
Var
	RangeStart :LiteralNode;
	RangeEnd :LiteralNode;
Begin
	Result := Nil;
	RangeStart := Self.ParseLiteral(HandlerNode, IsOpt);
	If RangeStart= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 23)) Then
			Exit;

	HandlerNode := RangeNode;
	RangeEnd := Self.ParseLiteral(HandlerNode, False);
	If RangeEnd= Nil Then
		Exit;

	Result := RangeNode.Create(RangeStart, RangeEnd);
End;

Function DelphiParser.ParseRangeType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeTypeNode;
Var
	Identifier :IdentifierNode;
	Range :RangeNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	Range := Self.ParseRange(HandlerNode, IsOpt);
	If Range= Nil Then
		Exit;

	HandlerNode := RangeTypeNode;
	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := RangeTypeNode.Create(Identifier, Range);
End;

Function DelphiParser.ParseRangeList(HandlerNode:ASTNodeClass; IsOpt:Boolean):RangeListNode;
Var
	Ranges :RangeNodeArray;
	Range :RangeNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Range := Self.ParseRange(HandlerNode, True);
	If Assigned(Range) Then
	Begin
		SetLength(Ranges, Succ(Length(Ranges)));
		Ranges[Pred(Length(Ranges))] := Range;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 7)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := RangeListNode.Create(Ranges);
End;

Function DelphiParser.ParseArray(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArrayNode;
Var
	RangeList :RangeListNode;
	Identifier :IdentifierNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 24)) Then
			Exit;

	HandlerNode := ArrayNode;
	If (Not NextToken().ExpectToken(HandlerNode, False, 25)) Then
			Exit;

	RangeList := Self.ParseRangeList(HandlerNode, False);
	If RangeList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 26)) Then
			Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 27)) Then
			Exit;

	Identifier := Self.ParseIdentifier(HandlerNode, False);
	If Identifier= Nil Then
		Exit;

	Result := ArrayNode.Create(RangeList, Identifier);
End;

Function DelphiParser.ParseArrayType(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArrayTypeNode;
Var
	Identifier :IdentifierNode;
	ArrayValue :ArrayNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	HandlerNode := ArrayTypeNode;
	ArrayValue := Self.ParseArray(HandlerNode, False);
	If ArrayValue= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := ArrayTypeNode.Create(Identifier, ArrayValue);
End;

Function DelphiParser.ParseRoutineDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineDeclarationNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseProcedureDeclaration(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseFunctionDeclaration(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(RoutineDeclarationNode, '');
End;

Function DelphiParser.ParseRoutineArgumentDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineArgumentDeclarationNode;
Var
	IdentifierList :IdentifierListNode;
	TypeID :IdentifierNode;
Begin
	Result := Nil;
	IdentifierList := Self.ParseIdentifierList(HandlerNode, IsOpt);
	If IdentifierList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 16)) Then
			Exit;

	HandlerNode := RoutineArgumentDeclarationNode;
	TypeID := Self.ParseIdentifier(HandlerNode, False);
	If TypeID= Nil Then
		Exit;

	Result := RoutineArgumentDeclarationNode.Create(IdentifierList, TypeID);
End;

Function DelphiParser.ParseRoutineArguments(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineArgumentsNode;
Var
	RoutineArgumentDeclarations :RoutineArgumentDeclarationNodeArray;
	RoutineArgumentDeclaration :RoutineArgumentDeclarationNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 21)) Then
			Exit;

	HandlerNode := RoutineArgumentsNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	RoutineArgumentDeclaration := Self.ParseRoutineArgumentDeclaration(HandlerNode, True);
	If Assigned(RoutineArgumentDeclaration) Then
	Begin
		SetLength(RoutineArgumentDeclarations, Succ(Length(RoutineArgumentDeclarations)));
		RoutineArgumentDeclarations[Pred(Length(RoutineArgumentDeclarations))] := RoutineArgumentDeclaration;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 10)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	If (Not NextToken().ExpectToken(HandlerNode, False, 22)) Then
			Exit;

	Result := RoutineArgumentsNode.Create(RoutineArgumentDeclarations);
End;

Function DelphiParser.ParseRoutineReturnType(HandlerNode:ASTNodeClass; IsOpt:Boolean):RoutineReturnTypeNode;
Var
	Identifier :IdentifierNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 16)) Then
			Exit;

	HandlerNode := RoutineReturnTypeNode;
	Identifier := Self.ParseIdentifier(HandlerNode, False);
	If Identifier= Nil Then
		Exit;

	Result := RoutineReturnTypeNode.Create(Identifier);
End;

Function DelphiParser.ParseFunctions(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionsNode;
Var
	RoutineDeclaration :RoutineDeclarationNode;
	RoutineArguments :RoutineArgumentsNode;
	RoutineReturnType :RoutineReturnTypeNode;
	Declarationss :DeclarationsNodeArray;
	Declarations :DeclarationsNode;
	Block :BlockNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	RoutineDeclaration := Self.ParseRoutineDeclaration(HandlerNode, IsOpt);
	If RoutineDeclaration= Nil Then
		Exit;

	HandlerNode := FunctionsNode;
	TempToken := Self.CurrentToken;
	Repeat
	RoutineArguments := Self.ParseRoutineArguments(HandlerNode, True);
	If RoutineArguments= Nil Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until True;

	TempToken := Self.CurrentToken;
	Repeat
	RoutineReturnType := Self.ParseRoutineReturnType(HandlerNode, True);
	If RoutineReturnType= Nil Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until True;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Declarations := Self.ParseDeclarations(HandlerNode, True);
	If Assigned(Declarations) Then
	Begin
		SetLength(Declarationss, Succ(Length(Declarationss)));
		Declarationss[Pred(Length(Declarationss))] := Declarations;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;
	Until (ListFinished);

	Block := Self.ParseBlock(HandlerNode, False);
	If Block= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 10)) Then
			Exit;

	Result := FunctionsNode.Create(RoutineDeclaration, RoutineArguments, RoutineReturnType, Declarationss, Block);
End;

Function DelphiParser.ParseProcedureDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):ProcedureDeclarationNode;
Var
	Identifier :IdentifierNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 28)) Then
			Exit;

	HandlerNode := ProcedureDeclarationNode;
	Identifier := Self.ParseIdentifier(HandlerNode, False);
	If Identifier= Nil Then
		Exit;

	Result := ProcedureDeclarationNode.Create(Identifier);
End;

Function DelphiParser.ParseFunctionDeclaration(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionDeclarationNode;
Var
	Identifier :IdentifierNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 29)) Then
			Exit;

	HandlerNode := FunctionDeclarationNode;
	Identifier := Self.ParseIdentifier(HandlerNode, False);
	If Identifier= Nil Then
		Exit;

	Result := FunctionDeclarationNode.Create(Identifier);
End;

Function DelphiParser.ParseAssignment(HandlerNode:ASTNodeClass; IsOpt:Boolean):AssignmentNode;
Var
	Identifier :IdentifierNode;
	Expression :ExpressionNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 31)) Then
			Exit;

	HandlerNode := AssignmentNode;
	Expression := Self.ParseExpression(HandlerNode, False);
	If Expression= Nil Then
		Exit;

	Result := AssignmentNode.Create(Identifier, Expression);
End;

Function DelphiParser.ParseWhileStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):WhileStatementNode;
Var
	Condition :ExpressionNode;
	Statements :StatementNodeArray;
	Statement :StatementNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 32)) Then
			Exit;

	HandlerNode := WhileStatementNode;
	Condition := Self.ParseExpression(HandlerNode, False);
	If Condition= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 33)) Then
			Exit;

	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Statement := Self.ParseStatement(HandlerNode, True);
	If Assigned(Statement) Then
	Begin
		SetLength(Statements, Succ(Length(Statements)));
		Statements[Pred(Length(Statements))] := Statement;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 10)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := WhileStatementNode.Create(Condition, Statements);
End;

Function DelphiParser.ParseRepeatStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):RepeatStatementNode;
Var
	Statements :StatementNodeArray;
	Statement :StatementNode;
	Condition :ExpressionNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 34)) Then
			Exit;

	HandlerNode := RepeatStatementNode;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Statement := Self.ParseStatement(HandlerNode, True);
	If Assigned(Statement) Then
	Begin
		SetLength(Statements, Succ(Length(Statements)));
		Statements[Pred(Length(Statements))] := Statement;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 10)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	If (Not NextToken().ExpectToken(HandlerNode, False, 35)) Then
			Exit;

	Condition := Self.ParseExpression(HandlerNode, False);
	If Condition= Nil Then
		Exit;

	Result := RepeatStatementNode.Create(Statements, Condition);
End;

Function DelphiParser.ParseForStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):ForStatementNode;
Var
	Assignment :AssignmentNode;
	Expression :ExpressionNode;
	Statements :StatementNodeArray;
	Statement :StatementNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 36)) Then
			Exit;

	HandlerNode := ForStatementNode;
	Assignment := Self.ParseAssignment(HandlerNode, False);
	If Assignment= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 37)) Then
			Exit;

	Expression := Self.ParseExpression(HandlerNode, False);
	If Expression= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 33)) Then
			Exit;

	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Statement := Self.ParseStatement(HandlerNode, True);
	If Assigned(Statement) Then
	Begin
		SetLength(Statements, Succ(Length(Statements)));
		Statements[Pred(Length(Statements))] := Statement;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 10)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := ForStatementNode.Create(Assignment, Expression, Statements);
End;

Function DelphiParser.ParseIfStatement(HandlerNode:ASTNodeClass; IsOpt:Boolean):IfStatementNode;
Var
	Expression :ExpressionNode;
	TrueStatement :StatementNode;
	FalseStatement :StatementNode;
	TempToken:LexerToken;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 38)) Then
			Exit;

	HandlerNode := IfStatementNode;
	Expression := Self.ParseExpression(HandlerNode, False);
	If Expression= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 39)) Then
			Exit;

	TrueStatement := Self.ParseStatement(HandlerNode, False);
	If TrueStatement= Nil Then
		Exit;

	TempToken := Self.CurrentToken;
	Repeat
	If (Not NextToken().ExpectToken(HandlerNode, True, 40)) Then
		Begin
			FalseStatement := Nil;
			Self.CurrentToken := TempToken;
			Break;
		End;

	FalseStatement := Self.ParseStatement(HandlerNode, True);
	If FalseStatement= Nil Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until True;

	Result := IfStatementNode.Create(Expression, TrueStatement, FalseStatement);
End;

Function DelphiParser.ParseExpressionList(HandlerNode:ASTNodeClass; IsOpt:Boolean):ExpressionListNode;
Var
	Expressions :ExpressionNodeArray;
	Expression :ExpressionNode;
	TempToken:LexerToken;
	ListFinished:Boolean;
Begin
	Result := Nil;
	TempToken := Self.CurrentToken;
	ListFinished := False;
	Repeat
	Expression := Self.ParseExpression(HandlerNode, True);
	If Assigned(Expression) Then
	Begin
		SetLength(Expressions, Succ(Length(Expressions)));
		Expressions[Pred(Length(Expressions))] := Expression;
		TempToken := Self.CurrentToken;
	End Else
	Begin
		ListFinished := True;
		Self.CurrentToken := TempToken;
	End;

	TempToken := Self.CurrentToken;
	If (Not NextToken().ExpectToken(HandlerNode, True, 7)) Then
	Begin
		Self.CurrentToken := TempToken;
		Break;
	End;

	Until (ListFinished);
	Result := ExpressionListNode.Create(Expressions);
End;

Function DelphiParser.ParseFunctionCall(HandlerNode:ASTNodeClass; IsOpt:Boolean):FunctionCallNode;
Var
	Identifier :IdentifierNode;
	ExpressionList :ExpressionListNode;
Begin
	Result := Nil;
	Identifier := Self.ParseIdentifier(HandlerNode, IsOpt);
	If Identifier= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 21)) Then
			Exit;

	HandlerNode := FunctionCallNode;
	ExpressionList := Self.ParseExpressionList(HandlerNode, False);
	If ExpressionList= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 22)) Then
			Exit;

	Result := FunctionCallNode.Create(Identifier, ExpressionList);
End;

Function DelphiParser.ParsePriorityExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):PriorityExpressionNode;
Var
	Expression :ExpressionNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 21)) Then
			Exit;

	HandlerNode := PriorityExpressionNode;
	Expression := Self.ParseExpression(HandlerNode, False);
	If Expression= Nil Then
		Exit;

	If (Not NextToken().ExpectToken(HandlerNode, False, 22)) Then
			Exit;

	Result := PriorityExpressionNode.Create(Expression);
End;

Function DelphiParser.ParseUnaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseUnaryOperatorPlus(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseUnaryOperatorMinus(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseUnaryOperatorAddressOf(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseUnaryOperatorNot(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(UnaryOperatorNode, '');
End;

Function DelphiParser.ParseUnaryExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryExpressionNode;
Var
	UnaryOperator :UnaryOperatorNode;
	Expression :ExpressionNode;
Begin
	Result := Nil;
	UnaryOperator := Self.ParseUnaryOperator(HandlerNode, IsOpt);
	If UnaryOperator= Nil Then
		Exit;

	Expression := Self.ParseExpression(HandlerNode, IsOpt);
	If Expression= Nil Then
		Exit;

	Result := UnaryExpressionNode.Create(UnaryOperator, Expression);
End;

Function DelphiParser.ParseUnaryOperatorPlus(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorPlusNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 41)) Then
			Exit;

	Result := UnaryOperatorPlusNode.Create();
End;

Function DelphiParser.ParseUnaryOperatorMinus(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorMinusNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 42)) Then
			Exit;

	Result := UnaryOperatorMinusNode.Create();
End;

Function DelphiParser.ParseUnaryOperatorAddressOf(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorAddressOfNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 43)) Then
			Exit;

	Result := UnaryOperatorAddressOfNode.Create();
End;

Function DelphiParser.ParseUnaryOperatorNot(HandlerNode:ASTNodeClass; IsOpt:Boolean):UnaryOperatorNotNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 44)) Then
			Exit;

	Result := UnaryOperatorNotNode.Create();
End;

Function DelphiParser.ParseBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):BinaryOperatorNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperator(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperator(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(BinaryOperatorNode, '');
End;

Function DelphiParser.ParseLogicBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorEquality(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorInequality(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorAnd(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorOr(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorXor(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorGreater(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorGreaterOrEqual(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorLess(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorLessOrEqual(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseLogicBinaryOperatorInside(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(LogicBinaryOperatorNode, '');
End;

Function DelphiParser.ParseLogicBinaryExpression(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryExpressionNode;
Var
	ExprA :ExpressionNode;
	LogicBinaryOperator :LogicBinaryOperatorNode;
	ExprB :ExpressionNode;
Begin
	Result := Nil;
	ExprA := Self.ParseExpression(HandlerNode, IsOpt);
	If ExprA= Nil Then
		Exit;

	LogicBinaryOperator := Self.ParseLogicBinaryOperator(HandlerNode, IsOpt);
	If LogicBinaryOperator= Nil Then
		Exit;

	ExprB := Self.ParseExpression(HandlerNode, IsOpt);
	If ExprB= Nil Then
		Exit;

	Result := LogicBinaryExpressionNode.Create(ExprA, LogicBinaryOperator, ExprB);
End;

Function DelphiParser.ParseLogicBinaryOperatorEquality(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorEqualityNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 14)) Then
			Exit;

	Result := LogicBinaryOperatorEqualityNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorInequality(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorInequalityNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 45)) Then
			Exit;

	Result := LogicBinaryOperatorInequalityNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorAnd(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorAndNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 46)) Then
			Exit;

	Result := LogicBinaryOperatorAndNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorOr(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorOrNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 47)) Then
			Exit;

	Result := LogicBinaryOperatorOrNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorXor(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorXorNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 48)) Then
			Exit;

	Result := LogicBinaryOperatorXorNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorGreater(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorGreaterNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 49)) Then
			Exit;

	Result := LogicBinaryOperatorGreaterNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorGreaterOrEqual(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorGreaterOrEqualNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 50)) Then
			Exit;

	Result := LogicBinaryOperatorGreaterOrEqualNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorLess(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorLessNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 51)) Then
			Exit;

	Result := LogicBinaryOperatorLessNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorLessOrEqual(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorLessOrEqualNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 52)) Then
			Exit;

	Result := LogicBinaryOperatorLessOrEqualNode.Create();
End;

Function DelphiParser.ParseLogicBinaryOperatorInside(HandlerNode:ASTNodeClass; IsOpt:Boolean):LogicBinaryOperatorInsideNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 53)) Then
			Exit;

	Result := LogicBinaryOperatorInsideNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperator(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorNode;
Var
	TempToken:LexerToken;
Begin
	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorPlus(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorMinus(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorDivision(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorModulus(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorProduct(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorShiftLeft(HandlerNode, True);
	If Assigned(Result) Then
	Begin
		Exit;
	End;
	Self.CurrentToken := TempToken;

	TempToken := Self.CurrentToken;
	Result := Self.ParseArithmeticBinaryOperatorShiftRight(HandlerNode, IsOpt);
	If (Result = Nil) And (Not IsOpt) Then
		ParsingExceptedError(ArithmeticBinaryOperatorNode, '');
End;

Function DelphiParser.ParseArithmeticBinaryOperatorPlus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorPlusNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 41)) Then
			Exit;

	Result := ArithmeticBinaryOperatorPlusNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorMinus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorMinusNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 42)) Then
			Exit;

	Result := ArithmeticBinaryOperatorMinusNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorDivision(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorDivisionNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 54)) Then
			Exit;

	Result := ArithmeticBinaryOperatorDivisionNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorModulus(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorModulusNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 55)) Then
			Exit;

	Result := ArithmeticBinaryOperatorModulusNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorProduct(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorProductNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 56)) Then
			Exit;

	Result := ArithmeticBinaryOperatorProductNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorShiftLeft(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorShiftLeftNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 57)) Then
			Exit;

	Result := ArithmeticBinaryOperatorShiftLeftNode.Create();
End;

Function DelphiParser.ParseArithmeticBinaryOperatorShiftRight(HandlerNode:ASTNodeClass; IsOpt:Boolean):ArithmeticBinaryOperatorShiftRightNode;
Begin
	Result := Nil;
	If (Not NextToken().ExpectToken(HandlerNode, IsOpt, 58)) Then
			Exit;

	Result := ArithmeticBinaryOperatorShiftRightNode.Create();
End;


End.
