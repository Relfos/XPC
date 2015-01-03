{
	XPC_ASTProcessor.pas
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
Unit XPC_ASTProcessor;

Interface

Uses XPC_ASTNodes;

Type
	ASTProcessor = Class
		Procedure Process(Node:ASTNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SourceNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TypeListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:MetaTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:DeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:DeclarationListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnitItemNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnitListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CompositeDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IdentifierListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TopLevelDeclarationSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InterfaceSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ImplementationSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StatementListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:BlockStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ProgramSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ProgramNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ConstantValueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExpressionListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IntLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ConstExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LabelDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ValueDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:VarDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ParamDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:VarParamDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ConstParamDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:OutParamDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ParametersSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FunctionAttributeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ProceduralTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CallableDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RoutineSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LibraryNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnitNode); Overload; Virtual; Abstract;
		Procedure Process(Node:PackageNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StructuredConstantNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ArrayConstNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FieldInitNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FieldInitListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RecordConstNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ConstIdentifierNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ScalarTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TypeClassNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IntegralTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IntegerTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SignedIntegerTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnsignedIntegerTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:BoolTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CharTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RealTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FloatTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:DoubleTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExtendedTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CurrencyTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StringTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FixedStringTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RangeTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EnumValueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EnumValueListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EnumTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:VariantTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:PointerTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:PropertySpecifiersNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ObjectSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CompositeTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FieldDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:PropertyDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ProceduralTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:MethodTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:MethodDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ObjectSectionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ArrayPropertyNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ClassTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ClassRefTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:MetaClassTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:OrdinalLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IntLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CharLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:BoolLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StringLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RealLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:NullLiteralNode); Overload; Virtual; Abstract;
		Procedure Process(Node:BinaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SetRangeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SubtractionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:AdditionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ProductNode); Overload; Virtual; Abstract;
		Procedure Process(Node:DivisionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:QuotientNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ModulusNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ShiftRightNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ShiftLeftNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LogicalBinaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ComparisonBinaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LogicalAndNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LogicalOrNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LogicalXorNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EqualNode); Overload; Virtual; Abstract;
		Procedure Process(Node:NotEqualNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LessThanNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LessOrEqualNode); Overload; Virtual; Abstract;
		Procedure Process(Node:GreaterThanNode); Overload; Virtual; Abstract;
		Procedure Process(Node:GreaterOrEqualNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TypeBinaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IsExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RuntimeCastNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SimpleUnaryExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnaryPlusNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnaryMinusNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LogicalNotNode); Overload; Virtual; Abstract;
		Procedure Process(Node:AddressLvalueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SetExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LvalueExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LValueAsExprNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LvalueExpressionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExprAsLvalueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StaticCastNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ArrayAccessNode); Overload; Virtual; Abstract;
		Procedure Process(Node:PointerDereferenceNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RoutineCallNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InheritedCallNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ObjectAccessNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IdentifierNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IdentifierListNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IdentifierStaticNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedLvalueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedIdNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedCallNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ConstDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EnumValueNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RoutineDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RoutineDefinitionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:MethodDefinitionNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CompositeDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ClassDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InterfaceTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InterfaceDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExternalDirectiveNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ImportDirectivesNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LabelStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:GotoStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:EmptyStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:BreakStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ContinueStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:AssignmentNode); Overload; Virtual; Abstract;
		Procedure Process(Node:IfStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExpressionStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CaseSelectorNode); Overload; Virtual; Abstract;
		Procedure Process(Node:CaseStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:LoopStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RepeatLoopNode); Overload; Virtual; Abstract;
		Procedure Process(Node:WhileLoopNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ForLoopNode); Overload; Virtual; Abstract;
		Procedure Process(Node:WithStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TryFinallyStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExceptionBlockNode); Overload; Virtual; Abstract;
		Procedure Process(Node:TryExceptStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RaiseStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:OnStatementNode); Overload; Virtual; Abstract;
		Procedure Process(Node:AssemblerBlockNode); Overload; Virtual; Abstract;
		Procedure Process(Node:InterfaceTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:StructuredTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RecordTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:RecordFieldDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ArrayTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:SetTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:FileTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:VariantDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:VarEntryDeclarationNode); Overload; Virtual; Abstract;
		Procedure Process(Node:ExportItemNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedVariableTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedIntegralTypeNode); Overload; Virtual; Abstract;
		Procedure Process(Node:UnresolvedOrdinalTypeNode); Overload; Virtual; Abstract;
	End;

Implementation

End.
