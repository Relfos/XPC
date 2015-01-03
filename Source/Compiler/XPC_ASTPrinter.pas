{
	XPC_ASTPrinter.pas
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

//Auto-generated, do not edit manually
Unit XPC_ASTPrinter;

Interface

Uses TERRA_Utils, XPC_ASTNodes, XPC_ASTProcessor;

Type
	ASTPrinter = Class(ASTProcessor)
		Procedure Print(Const S:AnsiString); Virtual;
		Procedure PrintNode(Node:ASTNode);
		Procedure Process(Node:ListNode); Override;
		Procedure Process(Node:SourceNode); Override;
		Procedure Process(Node:TypeNode); Override;
		Procedure Process(Node:TypeListNode); Override;
		Procedure Process(Node:MetaTypeNode); Override;
		Procedure Process(Node:DeclarationNode); Override;
		Procedure Process(Node:DeclarationListNode); Override;
		Procedure Process(Node:UnitItemNode); Override;
		Procedure Process(Node:UnitListNode); Override;
		Procedure Process(Node:StatementNode); Override;
		Procedure Process(Node:SectionNode); Override;
		Procedure Process(Node:TopLevelDeclarationSectionNode); Override;
		Procedure Process(Node:InterfaceSectionNode); Override;
		Procedure Process(Node:ImplementationSectionNode); Override;
		Procedure Process(Node:StatementListNode); Override;
		Procedure Process(Node:BlockStatementNode); Override;
		Procedure Process(Node:ProgramSectionNode); Override;
		Procedure Process(Node:ProgramNode); Override;
		Procedure Process(Node:ConstantValueNode); Override;
		Procedure Process(Node:ExpressionNode); Override;
		Procedure Process(Node:ExpressionListNode); Override;
		Procedure Process(Node:ConstExpressionNode); Override;
		Procedure Process(Node:LabelDeclarationNode); Override;
		Procedure Process(Node:ValueDeclarationNode); Override;
		Procedure Process(Node:VarDeclarationNode); Override;
		Procedure Process(Node:ParamDeclarationNode); Override;
		Procedure Process(Node:VarParamDeclarationNode); Override;
		Procedure Process(Node:ConstParamDeclarationNode); Override;
		Procedure Process(Node:OutParamDeclarationNode); Override;
		Procedure Process(Node:ParametersSectionNode); Override;
		Procedure Process(Node:FunctionAttributeNode); Override;
		Procedure Process(Node:CallableDeclarationNode); Override;
		Procedure Process(Node:RoutineSectionNode); Override;
		Procedure Process(Node:LibraryNode); Override;
		Procedure Process(Node:UnitNode); Override;
		Procedure Process(Node:PackageNode); Override;
		Procedure Process(Node:StructuredConstantNode); Override;
		Procedure Process(Node:ArrayConstNode); Override;
		Procedure Process(Node:FieldInitNode); Override;
		Procedure Process(Node:FieldInitListNode); Override;
		Procedure Process(Node:RecordConstNode); Override;
		Procedure Process(Node:ConstIdentifierNode); Override;
		Procedure Process(Node:ScalarTypeNode); Override;
		Procedure Process(Node:IntegralTypeNode); Override;
		Procedure Process(Node:IntegerTypeNode); Override;
		Procedure Process(Node:SignedIntegerTypeNode); Override;
		Procedure Process(Node:UnsignedIntegerTypeNode); Override;
		Procedure Process(Node:BoolTypeNode); Override;
		Procedure Process(Node:CharTypeNode); Override;
		Procedure Process(Node:RealTypeNode); Override;
		Procedure Process(Node:FloatTypeNode); Override;
		Procedure Process(Node:DoubleTypeNode); Override;
		Procedure Process(Node:ExtendedTypeNode); Override;
		Procedure Process(Node:CurrencyTypeNode); Override;
		Procedure Process(Node:StringTypeNode); Override;
		Procedure Process(Node:FixedStringTypeNode); Override;
		Procedure Process(Node:RangeTypeNode); Override;
		Procedure Process(Node:EnumValueListNode); Override;
		Procedure Process(Node:EnumTypeNode); Override;
		Procedure Process(Node:VariantTypeNode); Override;
		Procedure Process(Node:PointerTypeNode); Override;
		Procedure Process(Node:PropertySpecifiersNode); Override;
		Procedure Process(Node:CompositeTypeNode); Override;
		Procedure Process(Node:FieldDeclarationNode); Override;
		Procedure Process(Node:PropertyDeclarationNode); Override;
		Procedure Process(Node:ProceduralTypeNode); Override;
		Procedure Process(Node:MethodTypeNode); Override;
		Procedure Process(Node:MethodDeclarationNode); Override;
		Procedure Process(Node:ObjectSectionNode); Override;
		Procedure Process(Node:ArrayPropertyNode); Override;
		Procedure Process(Node:ClassTypeNode); Override;
		Procedure Process(Node:ClassRefTypeNode); Override;
		Procedure Process(Node:MetaClassTypeNode); Override;
		Procedure Process(Node:LiteralNode); Override;
		Procedure Process(Node:OrdinalLiteralNode); Override;
		Procedure Process(Node:IntLiteralNode); Override;
		Procedure Process(Node:CharLiteralNode); Override;
		Procedure Process(Node:BoolLiteralNode); Override;
		Procedure Process(Node:StringLiteralNode); Override;
		Procedure Process(Node:RealLiteralNode); Override;
		Procedure Process(Node:NullLiteralNode); Override;
		Procedure Process(Node:BinaryExpressionNode); Override;
		Procedure Process(Node:InExpressionNode); Override;
		Procedure Process(Node:SetRangeNode); Override;
		Procedure Process(Node:SubtractionNode); Override;
		Procedure Process(Node:AdditionNode); Override;
		Procedure Process(Node:ProductNode); Override;
		Procedure Process(Node:DivisionNode); Override;
		Procedure Process(Node:QuotientNode); Override;
		Procedure Process(Node:ModulusNode); Override;
		Procedure Process(Node:ShiftRightNode); Override;
		Procedure Process(Node:ShiftLeftNode); Override;
		Procedure Process(Node:LogicalBinaryExpressionNode); Override;
		Procedure Process(Node:ComparisonBinaryExpressionNode); Override;
		Procedure Process(Node:LogicalAndNode); Override;
		Procedure Process(Node:LogicalOrNode); Override;
		Procedure Process(Node:LogicalXorNode); Override;
		Procedure Process(Node:EqualNode); Override;
		Procedure Process(Node:NotEqualNode); Override;
		Procedure Process(Node:LessThanNode); Override;
		Procedure Process(Node:LessOrEqualNode); Override;
		Procedure Process(Node:GreaterThanNode); Override;
		Procedure Process(Node:GreaterOrEqualNode); Override;
		Procedure Process(Node:TypeBinaryExpressionNode); Override;
		Procedure Process(Node:IsExpressionNode); Override;
		Procedure Process(Node:RuntimeCastNode); Override;
		Procedure Process(Node:UnaryExpressionNode); Override;
		Procedure Process(Node:SimpleUnaryExpressionNode); Override;
		Procedure Process(Node:UnaryPlusNode); Override;
		Procedure Process(Node:UnaryMinusNode); Override;
		Procedure Process(Node:LogicalNotNode); Override;
		Procedure Process(Node:AddressLvalueNode); Override;
		Procedure Process(Node:SetExpressionNode); Override;
		Procedure Process(Node:LValueAsExprNode); Override;
		Procedure Process(Node:LvalueExpressionNode); Override;
		Procedure Process(Node:ExprAsLvalueNode); Override;
		Procedure Process(Node:StaticCastNode); Override;
		Procedure Process(Node:ArrayAccessNode); Override;
		Procedure Process(Node:PointerDereferenceNode); Override;
		Procedure Process(Node:RoutineCallNode); Override;
		Procedure Process(Node:InheritedCallNode); Override;
		Procedure Process(Node:ObjectAccessNode); Override;
		Procedure Process(Node:IdentifierNode); Override;
		Procedure Process(Node:IdentifierListNode); Override;
		Procedure Process(Node:IdentifierStaticNode); Override;
		Procedure Process(Node:UnresolvedLvalueNode); Override;
		Procedure Process(Node:UnresolvedIdNode); Override;
		Procedure Process(Node:UnresolvedCallNode); Override;
		Procedure Process(Node:ConstDeclarationNode); Override;
		Procedure Process(Node:EnumValueNode); Override;
		Procedure Process(Node:RoutineDeclarationNode); Override;
		Procedure Process(Node:RoutineDefinitionNode); Override;
		Procedure Process(Node:MethodDefinitionNode); Override;
		Procedure Process(Node:CompositeDeclarationNode); Override;
		Procedure Process(Node:ClassDeclarationNode); Override;
		Procedure Process(Node:InterfaceDeclarationNode); Override;
		Procedure Process(Node:ExternalDirectiveNode); Override;
		Procedure Process(Node:ImportDirectivesNode); Override;
		Procedure Process(Node:LabelStatementNode); Override;
		Procedure Process(Node:GotoStatementNode); Override;
		Procedure Process(Node:EmptyStatementNode); Override;
		Procedure Process(Node:BreakStatementNode); Override;
		Procedure Process(Node:ContinueStatementNode); Override;
		Procedure Process(Node:AssignmentNode); Override;
		Procedure Process(Node:IfStatementNode); Override;
		Procedure Process(Node:ExpressionStatementNode); Override;
		Procedure Process(Node:CaseSelectorNode); Override;
		Procedure Process(Node:CaseStatementNode); Override;
		Procedure Process(Node:LoopStatementNode); Override;
		Procedure Process(Node:RepeatLoopNode); Override;
		Procedure Process(Node:WhileLoopNode); Override;
		Procedure Process(Node:ForLoopNode); Override;
		Procedure Process(Node:WithStatementNode); Override;
		Procedure Process(Node:TryFinallyStatementNode); Override;
		Procedure Process(Node:ExceptionBlockNode); Override;
		Procedure Process(Node:TryExceptStatementNode); Override;
		Procedure Process(Node:RaiseStatementNode); Override;
		Procedure Process(Node:OnStatementNode); Override;
		Procedure Process(Node:AssemblerBlockNode); Override;
		Procedure Process(Node:InterfaceTypeNode); Override;
		Procedure Process(Node:StructuredTypeNode); Override;
		Procedure Process(Node:RecordTypeNode); Override;
		Procedure Process(Node:RecordFieldDeclarationNode); Override;
		Procedure Process(Node:ArrayTypeNode); Override;
		Procedure Process(Node:SetTypeNode); Override;
		Procedure Process(Node:FileTypeNode); Override;
		Procedure Process(Node:VariantDeclarationNode); Override;
		Procedure Process(Node:VarEntryDeclarationNode); Override;
		Procedure Process(Node:ExportItemNode); Override;
		Procedure Process(Node:UnresolvedNode); Override;
		Procedure Process(Node:UnresolvedTypeNode); Override;
		Procedure Process(Node:UnresolvedVariableTypeNode); Override;
		Procedure Process(Node:UnresolvedIntegralTypeNode); Override;
		Procedure Process(Node:UnresolvedOrdinalTypeNode); Override;

	End;

Implementation

Uses TERRA_OS;

Procedure ASTPrinter.Print(Const S:AnsiString); 
Begin
	Write(S);
End;

Procedure ASTPrinter.PrintNode(Node:ASTNode);
Begin
	If (Node = Nil) Then
		Exit;
	Print(Spaces(Node.Level)+Node.ClassName+CrLf);
	Dispatch(Node);
End;

Procedure ASTPrinter.Process(Node:ListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:SourceNode);
Begin
End;

Procedure ASTPrinter.Process(Node:TypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:TypeListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:MetaTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:DeclarationNode);
Begin
	PrintNode(Node.DeclType);
End;

Procedure ASTPrinter.Process(Node:DeclarationListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:UnitItemNode);
Begin
	PrintNode(Node.DeclType);
End;

Procedure ASTPrinter.Process(Node:UnitListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:StatementNode);
Begin
End;

Procedure ASTPrinter.Process(Node:SectionNode);
Begin
	PrintNode(Node.Decls);
End;

Procedure ASTPrinter.Process(Node:TopLevelDeclarationSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.UsesList);
End;

Procedure ASTPrinter.Process(Node:InterfaceSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.UsesList);
End;

Procedure ASTPrinter.Process(Node:ImplementationSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.UsesList);
End;

Procedure ASTPrinter.Process(Node:StatementListNode);
Var I:Integer;
Begin
	If Assigned(Node.List) Then
		For I:=0 To Pred(Node.List.Count) Do
			Dispatch(Node.List.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:BlockStatementNode);
Begin
	PrintNode(Node.List);
End;

Procedure ASTPrinter.Process(Node:ProgramSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.UsesList);
	PrintNode(Node.Block);
End;

Procedure ASTPrinter.Process(Node:ProgramNode);
Begin
	PrintNode(Node.Section);
End;

Procedure ASTPrinter.Process(Node:ConstantValueNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ExpressionNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:ExpressionListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:ConstExpressionNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:LabelDeclarationNode);
Begin
	PrintNode(Node.DeclType);
End;

Procedure ASTPrinter.Process(Node:ValueDeclarationNode);
Begin
	PrintNode(Node.DeclType);
End;

Procedure ASTPrinter.Process(Node:VarDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:ParamDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:VarParamDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:ConstParamDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:OutParamDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:ParametersSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.ReturnVar);
End;

Procedure ASTPrinter.Process(Node:FunctionAttributeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:CallableDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.SignatureType);
	PrintNode(Node.ResultType);
	PrintNode(Node.Directives);
End;

Procedure ASTPrinter.Process(Node:RoutineSectionNode);
Begin
	PrintNode(Node.Decls);
End;

Procedure ASTPrinter.Process(Node:LibraryNode);
Begin
	PrintNode(Node.Section);
End;

Procedure ASTPrinter.Process(Node:UnitNode);
Begin
	PrintNode(Node.Interfaces);
	PrintNode(Node.Implements);
	PrintNode(Node.Inits);
	PrintNode(Node.Final);
End;

Procedure ASTPrinter.Process(Node:PackageNode);
Begin
	PrintNode(Node.Requires);
	PrintNode(Node.Contains);
End;

Procedure ASTPrinter.Process(Node:StructuredConstantNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.ExprList);
End;

Procedure ASTPrinter.Process(Node:ArrayConstNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.ExprList);
End;

Procedure ASTPrinter.Process(Node:FieldInitNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:FieldInitListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:RecordConstNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.ExprList);
End;

Procedure ASTPrinter.Process(Node:ConstIdentifierNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:ScalarTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:IntegralTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:IntegerTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:SignedIntegerTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:UnsignedIntegerTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:BoolTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:CharTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:RealTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:FloatTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:DoubleTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ExtendedTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:CurrencyTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:StringTypeNode);
Begin
	PrintNode(Node.Length);
End;

Procedure ASTPrinter.Process(Node:FixedStringTypeNode);
Begin
	PrintNode(Node.Length);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:RangeTypeNode);
Begin
	PrintNode(Node.Min);
	PrintNode(Node.Max);
End;

Procedure ASTPrinter.Process(Node:EnumValueListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:EnumTypeNode);
Begin
	PrintNode(Node.List);
End;

Procedure ASTPrinter.Process(Node:VariantTypeNode);
Begin
	PrintNode(Node.ActualType);
End;

Procedure ASTPrinter.Process(Node:PointerTypeNode);
Begin
	PrintNode(Node.PointedType);
End;

Procedure ASTPrinter.Process(Node:PropertySpecifiersNode);
Begin
	PrintNode(Node.Index);
	PrintNode(Node.Stored);
	PrintNode(Node.Default);
End;

Procedure ASTPrinter.Process(Node:CompositeTypeNode);
Begin
	PrintNode(Node.Heritage);
	PrintNode(Node.Section);
	PrintNode(Node.Decl);
End;

Procedure ASTPrinter.Process(Node:FieldDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.DeclaringObject);
End;

Procedure ASTPrinter.Process(Node:PropertyDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.DeclaringObject);
	PrintNode(Node.Specifiers);
End;

Procedure ASTPrinter.Process(Node:ProceduralTypeNode);
Begin
	PrintNode(Node.Params);
	PrintNode(Node.FuncRet);
	PrintNode(Node.Directives);
End;

Procedure ASTPrinter.Process(Node:MethodTypeNode);
Begin
	PrintNode(Node.Params);
	PrintNode(Node.FuncRet);
	PrintNode(Node.Directives);
End;

Procedure ASTPrinter.Process(Node:MethodDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.SignatureType);
	PrintNode(Node.ResultType);
	PrintNode(Node.Directives);
	PrintNode(Node.DeclaringObject);
End;

Procedure ASTPrinter.Process(Node:ObjectSectionNode);
Begin
	PrintNode(Node.Decls);
	PrintNode(Node.Fields);
	PrintNode(Node.Properties);
	PrintNode(Node.DeclaringObject);
End;

Procedure ASTPrinter.Process(Node:ArrayPropertyNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.DeclaringObject);
	PrintNode(Node.Specifiers);
	PrintNode(Node.Indexes);
End;

Procedure ASTPrinter.Process(Node:ClassTypeNode);
Begin
	PrintNode(Node.Heritage);
	PrintNode(Node.Section);
	PrintNode(Node.Decl);
	PrintNode(Node._Self);
End;

Procedure ASTPrinter.Process(Node:ClassRefTypeNode);
Begin
	PrintNode(Node.Heritage);
	PrintNode(Node.Section);
	PrintNode(Node.Decl);
	PrintNode(Node._Self);
	PrintNode(Node.RefType);
End;

Procedure ASTPrinter.Process(Node:MetaClassTypeNode);
Begin
	PrintNode(Node.BaseType);
End;

Procedure ASTPrinter.Process(Node:LiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:OrdinalLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:IntLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:CharLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:BoolLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:StringLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:RealLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:NullLiteralNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:BinaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:InExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
	PrintNode(Node.Expr);
	PrintNode(Node._Set);
End;

Procedure ASTPrinter.Process(Node:SetRangeNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
	PrintNode(Node.Range);
End;

Procedure ASTPrinter.Process(Node:SubtractionNode);
Begin
End;

Procedure ASTPrinter.Process(Node:AdditionNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ProductNode);
Begin
End;

Procedure ASTPrinter.Process(Node:DivisionNode);
Begin
End;

Procedure ASTPrinter.Process(Node:QuotientNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ModulusNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ShiftRightNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ShiftLeftNode);
Begin
End;

Procedure ASTPrinter.Process(Node:LogicalBinaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:ComparisonBinaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:LogicalAndNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:LogicalOrNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:LogicalXorNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:EqualNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:NotEqualNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:LessThanNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:LessOrEqualNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:GreaterThanNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:GreaterOrEqualNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
End;

Procedure ASTPrinter.Process(Node:TypeBinaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
	PrintNode(Node.Expr);
	PrintNode(Node.Types);
End;

Procedure ASTPrinter.Process(Node:IsExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
	PrintNode(Node.Expr);
	PrintNode(Node.Types);
End;

Procedure ASTPrinter.Process(Node:RuntimeCastNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Left);
	PrintNode(Node.Right);
	PrintNode(Node.Expr);
	PrintNode(Node.Types);
End;

Procedure ASTPrinter.Process(Node:UnaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:SimpleUnaryExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:UnaryPlusNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:UnaryMinusNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:LogicalNotNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:AddressLvalueNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:SetExpressionNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Elements);
End;

Procedure ASTPrinter.Process(Node:LValueAsExprNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.lval);
End;

Procedure ASTPrinter.Process(Node:LvalueExpressionNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:ExprAsLvalueNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:StaticCastNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.CastType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:ArrayAccessNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.LValue);
	PrintNode(Node.Acessors);
	PrintNode(Node._Array);
End;

Procedure ASTPrinter.Process(Node:PointerDereferenceNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:RoutineCallNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Func);
	PrintNode(Node.Args);
End;

Procedure ASTPrinter.Process(Node:InheritedCallNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Func);
	PrintNode(Node.Args);
End;

Procedure ASTPrinter.Process(Node:ObjectAccessNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Obj);
End;

Procedure ASTPrinter.Process(Node:IdentifierNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Decl);
End;

Procedure ASTPrinter.Process(Node:IdentifierListNode);
Var I:Integer;
Begin
	For I:=0 To Pred(Node.Count) Do
		Dispatch(Node.Elements[I]);
End;

Procedure ASTPrinter.Process(Node:IdentifierStaticNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Decl);
End;

Procedure ASTPrinter.Process(Node:UnresolvedLvalueNode);
Begin
	PrintNode(Node.ForcedType);
End;

Procedure ASTPrinter.Process(Node:UnresolvedIdNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.ID);
End;

Procedure ASTPrinter.Process(Node:UnresolvedCallNode);
Begin
	PrintNode(Node.ForcedType);
	PrintNode(Node.Func);
	PrintNode(Node.Args);
End;

Procedure ASTPrinter.Process(Node:ConstDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:EnumValueNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Init);
End;

Procedure ASTPrinter.Process(Node:RoutineDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.SignatureType);
	PrintNode(Node.ResultType);
	PrintNode(Node.Directives);
End;

Procedure ASTPrinter.Process(Node:RoutineDefinitionNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.SignatureType);
	PrintNode(Node.ResultType);
	PrintNode(Node.Directives);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:MethodDefinitionNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.SignatureType);
	PrintNode(Node.ResultType);
	PrintNode(Node.Directives);
	PrintNode(Node.DeclaringObject);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:CompositeDeclarationNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ClassDeclarationNode);
Begin
End;

Procedure ASTPrinter.Process(Node:InterfaceDeclarationNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ExternalDirectiveNode);
Begin
	PrintNode(Node._File);
	PrintNode(Node.Name);
End;

Procedure ASTPrinter.Process(Node:ImportDirectivesNode);
Begin
	PrintNode(Node.ImportDir);
	PrintNode(Node._external);
End;

Procedure ASTPrinter.Process(Node:LabelStatementNode);
Begin
	PrintNode(Node.Statement);
End;

Procedure ASTPrinter.Process(Node:GotoStatementNode);
Begin
End;

Procedure ASTPrinter.Process(Node:EmptyStatementNode);
Begin
End;

Procedure ASTPrinter.Process(Node:BreakStatementNode);
Begin
End;

Procedure ASTPrinter.Process(Node:ContinueStatementNode);
Begin
End;

Procedure ASTPrinter.Process(Node:AssignmentNode);
Begin
	PrintNode(Node.lvalue);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:IfStatementNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.ThenBlock);
	PrintNode(Node.ElseBlock);
End;

Procedure ASTPrinter.Process(Node:ExpressionStatementNode);
Begin
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:CaseSelectorNode);
Begin
	PrintNode(Node.List);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:CaseStatementNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.Selectors);
	PrintNode(Node.CaseElse);
End;

Procedure ASTPrinter.Process(Node:LoopStatementNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:RepeatLoopNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:WhileLoopNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:ForLoopNode);
Begin
	PrintNode(Node.Condition);
	PrintNode(Node.Body);
	PrintNode(Node._var);
	PrintNode(Node.Start);
	PrintNode(Node._End);
End;

Procedure ASTPrinter.Process(Node:WithStatementNode);
Begin
	PrintNode(Node._With);
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:TryFinallyStatementNode);
Begin
	PrintNode(Node.Body);
	PrintNode(Node.Final);
End;

Procedure ASTPrinter.Process(Node:ExceptionBlockNode);
Begin
	PrintNode(Node.onList);
	PrintNode(Node.Default);
End;

Procedure ASTPrinter.Process(Node:TryExceptStatementNode);
Begin
	PrintNode(Node.Body);
	PrintNode(Node.Final);
End;

Procedure ASTPrinter.Process(Node:RaiseStatementNode);
Begin
	PrintNode(Node.LValue);
	PrintNode(Node.Expr);
End;

Procedure ASTPrinter.Process(Node:OnStatementNode);
Begin
	PrintNode(Node.Body);
End;

Procedure ASTPrinter.Process(Node:AssemblerBlockNode);
Begin
	PrintNode(Node.List);
End;

Procedure ASTPrinter.Process(Node:InterfaceTypeNode);
Begin
	PrintNode(Node.Heritage);
	PrintNode(Node.Section);
	PrintNode(Node.Decl);
	PrintNode(Node.Ssec);
	PrintNode(Node.Guid);
End;

Procedure ASTPrinter.Process(Node:StructuredTypeNode);
Begin
	PrintNode(Node.BaseType);
End;

Procedure ASTPrinter.Process(Node:RecordTypeNode);
Begin
	PrintNode(Node.BaseType);
	PrintNode(Node.CompTypes);
End;

Procedure ASTPrinter.Process(Node:RecordFieldDeclarationNode);
Begin
	PrintNode(Node.DeclType);
End;

Procedure ASTPrinter.Process(Node:ArrayTypeNode);
Begin
	PrintNode(Node.BaseType);
End;

Procedure ASTPrinter.Process(Node:SetTypeNode);
Begin
	PrintNode(Node.BaseType);
End;

Procedure ASTPrinter.Process(Node:FileTypeNode);
Begin
	PrintNode(Node.BaseType);
End;

Procedure ASTPrinter.Process(Node:VariantDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.Fields);
End;

Procedure ASTPrinter.Process(Node:VarEntryDeclarationNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.TagValue);
	PrintNode(Node.Fields);
End;

Procedure ASTPrinter.Process(Node:ExportItemNode);
Begin
	PrintNode(Node.DeclType);
	PrintNode(Node.FormalParams);
End;

Procedure ASTPrinter.Process(Node:UnresolvedNode);
Begin
End;

Procedure ASTPrinter.Process(Node:UnresolvedTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:UnresolvedVariableTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:UnresolvedIntegralTypeNode);
Begin
End;

Procedure ASTPrinter.Process(Node:UnresolvedOrdinalTypeNode);
Begin
End;



End.
