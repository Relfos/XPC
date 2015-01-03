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
		Procedure Dispatch(Node:ASTNode);
		Procedure Process(Node:ListNode); Overload; Virtual;
		Procedure Process(Node:SourceNode); Overload; Virtual;
		Procedure Process(Node:TypeNode); Overload; Virtual;
		Procedure Process(Node:TypeListNode); Overload; Virtual;
		Procedure Process(Node:MetaTypeNode); Overload; Virtual;
		Procedure Process(Node:DeclarationNode); Overload; Virtual;
		Procedure Process(Node:DeclarationListNode); Overload; Virtual;
		Procedure Process(Node:UnitItemNode); Overload; Virtual;
		Procedure Process(Node:UnitListNode); Overload; Virtual;
		Procedure Process(Node:StatementNode); Overload; Virtual;
		Procedure Process(Node:SectionNode); Overload; Virtual;
		Procedure Process(Node:TopLevelDeclarationSectionNode); Overload; Virtual;
		Procedure Process(Node:InterfaceSectionNode); Overload; Virtual;
		Procedure Process(Node:ImplementationSectionNode); Overload; Virtual;
		Procedure Process(Node:StatementListNode); Overload; Virtual;
		Procedure Process(Node:BlockStatementNode); Overload; Virtual;
		Procedure Process(Node:ProgramSectionNode); Overload; Virtual;
		Procedure Process(Node:ProgramNode); Overload; Virtual;
		Procedure Process(Node:ConstantValueNode); Overload; Virtual;
		Procedure Process(Node:ExpressionNode); Overload; Virtual;
		Procedure Process(Node:ExpressionListNode); Overload; Virtual;
		Procedure Process(Node:ConstExpressionNode); Overload; Virtual;
		Procedure Process(Node:LabelDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ValueDeclarationNode); Overload; Virtual;
		Procedure Process(Node:VarDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ParamDeclarationNode); Overload; Virtual;
		Procedure Process(Node:VarParamDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ConstParamDeclarationNode); Overload; Virtual;
		Procedure Process(Node:OutParamDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ParametersSectionNode); Overload; Virtual;
		Procedure Process(Node:FunctionAttributeNode); Overload; Virtual;
		Procedure Process(Node:CallableDeclarationNode); Overload; Virtual;
		Procedure Process(Node:RoutineSectionNode); Overload; Virtual;
		Procedure Process(Node:LibraryNode); Overload; Virtual;
		Procedure Process(Node:UnitNode); Overload; Virtual;
		Procedure Process(Node:PackageNode); Overload; Virtual;
		Procedure Process(Node:StructuredConstantNode); Overload; Virtual;
		Procedure Process(Node:ArrayConstNode); Overload; Virtual;
		Procedure Process(Node:FieldInitNode); Overload; Virtual;
		Procedure Process(Node:FieldInitListNode); Overload; Virtual;
		Procedure Process(Node:RecordConstNode); Overload; Virtual;
		Procedure Process(Node:ConstIdentifierNode); Overload; Virtual;
		Procedure Process(Node:ScalarTypeNode); Overload; Virtual;
		Procedure Process(Node:IntegralTypeNode); Overload; Virtual;
		Procedure Process(Node:IntegerTypeNode); Overload; Virtual;
		Procedure Process(Node:SignedIntegerTypeNode); Overload; Virtual;
		Procedure Process(Node:UnsignedIntegerTypeNode); Overload; Virtual;
		Procedure Process(Node:BoolTypeNode); Overload; Virtual;
		Procedure Process(Node:CharTypeNode); Overload; Virtual;
		Procedure Process(Node:RealTypeNode); Overload; Virtual;
		Procedure Process(Node:FloatTypeNode); Overload; Virtual;
		Procedure Process(Node:DoubleTypeNode); Overload; Virtual;
		Procedure Process(Node:ExtendedTypeNode); Overload; Virtual;
		Procedure Process(Node:CurrencyTypeNode); Overload; Virtual;
		Procedure Process(Node:StringTypeNode); Overload; Virtual;
		Procedure Process(Node:FixedStringTypeNode); Overload; Virtual;
		Procedure Process(Node:RangeTypeNode); Overload; Virtual;
		Procedure Process(Node:EnumValueListNode); Overload; Virtual;
		Procedure Process(Node:EnumTypeNode); Overload; Virtual;
		Procedure Process(Node:VariantTypeNode); Overload; Virtual;
		Procedure Process(Node:PointerTypeNode); Overload; Virtual;
		Procedure Process(Node:PropertySpecifiersNode); Overload; Virtual;
		Procedure Process(Node:CompositeTypeNode); Overload; Virtual;
		Procedure Process(Node:FieldDeclarationNode); Overload; Virtual;
		Procedure Process(Node:PropertyDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ProceduralTypeNode); Overload; Virtual;
		Procedure Process(Node:MethodTypeNode); Overload; Virtual;
		Procedure Process(Node:MethodDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ObjectSectionNode); Overload; Virtual;
		Procedure Process(Node:ArrayPropertyNode); Overload; Virtual;
		Procedure Process(Node:ClassTypeNode); Overload; Virtual;
		Procedure Process(Node:ClassRefTypeNode); Overload; Virtual;
		Procedure Process(Node:MetaClassTypeNode); Overload; Virtual;
		Procedure Process(Node:LiteralNode); Overload; Virtual;
		Procedure Process(Node:OrdinalLiteralNode); Overload; Virtual;
		Procedure Process(Node:IntLiteralNode); Overload; Virtual;
		Procedure Process(Node:CharLiteralNode); Overload; Virtual;
		Procedure Process(Node:BoolLiteralNode); Overload; Virtual;
		Procedure Process(Node:StringLiteralNode); Overload; Virtual;
		Procedure Process(Node:RealLiteralNode); Overload; Virtual;
		Procedure Process(Node:NullLiteralNode); Overload; Virtual;
		Procedure Process(Node:BinaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:InExpressionNode); Overload; Virtual;
		Procedure Process(Node:SetRangeNode); Overload; Virtual;
		Procedure Process(Node:SubtractionNode); Overload; Virtual;
		Procedure Process(Node:AdditionNode); Overload; Virtual;
		Procedure Process(Node:ProductNode); Overload; Virtual;
		Procedure Process(Node:DivisionNode); Overload; Virtual;
		Procedure Process(Node:QuotientNode); Overload; Virtual;
		Procedure Process(Node:ModulusNode); Overload; Virtual;
		Procedure Process(Node:ShiftRightNode); Overload; Virtual;
		Procedure Process(Node:ShiftLeftNode); Overload; Virtual;
		Procedure Process(Node:LogicalBinaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:ComparisonBinaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:LogicalAndNode); Overload; Virtual;
		Procedure Process(Node:LogicalOrNode); Overload; Virtual;
		Procedure Process(Node:LogicalXorNode); Overload; Virtual;
		Procedure Process(Node:EqualNode); Overload; Virtual;
		Procedure Process(Node:NotEqualNode); Overload; Virtual;
		Procedure Process(Node:LessThanNode); Overload; Virtual;
		Procedure Process(Node:LessOrEqualNode); Overload; Virtual;
		Procedure Process(Node:GreaterThanNode); Overload; Virtual;
		Procedure Process(Node:GreaterOrEqualNode); Overload; Virtual;
		Procedure Process(Node:TypeBinaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:IsExpressionNode); Overload; Virtual;
		Procedure Process(Node:RuntimeCastNode); Overload; Virtual;
		Procedure Process(Node:UnaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:SimpleUnaryExpressionNode); Overload; Virtual;
		Procedure Process(Node:UnaryPlusNode); Overload; Virtual;
		Procedure Process(Node:UnaryMinusNode); Overload; Virtual;
		Procedure Process(Node:LogicalNotNode); Overload; Virtual;
		Procedure Process(Node:AddressLvalueNode); Overload; Virtual;
		Procedure Process(Node:SetExpressionNode); Overload; Virtual;
		Procedure Process(Node:LValueAsExprNode); Overload; Virtual;
		Procedure Process(Node:LvalueExpressionNode); Overload; Virtual;
		Procedure Process(Node:ExprAsLvalueNode); Overload; Virtual;
		Procedure Process(Node:StaticCastNode); Overload; Virtual;
		Procedure Process(Node:ArrayAccessNode); Overload; Virtual;
		Procedure Process(Node:PointerDereferenceNode); Overload; Virtual;
		Procedure Process(Node:RoutineCallNode); Overload; Virtual;
		Procedure Process(Node:InheritedCallNode); Overload; Virtual;
		Procedure Process(Node:ObjectAccessNode); Overload; Virtual;
		Procedure Process(Node:IdentifierNode); Overload; Virtual;
		Procedure Process(Node:IdentifierListNode); Overload; Virtual;
		Procedure Process(Node:IdentifierStaticNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedLvalueNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedIdNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedCallNode); Overload; Virtual;
		Procedure Process(Node:ConstDeclarationNode); Overload; Virtual;
		Procedure Process(Node:EnumValueNode); Overload; Virtual;
		Procedure Process(Node:RoutineDeclarationNode); Overload; Virtual;
		Procedure Process(Node:RoutineDefinitionNode); Overload; Virtual;
		Procedure Process(Node:MethodDefinitionNode); Overload; Virtual;
		Procedure Process(Node:CompositeDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ClassDeclarationNode); Overload; Virtual;
		Procedure Process(Node:InterfaceDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ExternalDirectiveNode); Overload; Virtual;
		Procedure Process(Node:ImportDirectivesNode); Overload; Virtual;
		Procedure Process(Node:LabelStatementNode); Overload; Virtual;
		Procedure Process(Node:GotoStatementNode); Overload; Virtual;
		Procedure Process(Node:EmptyStatementNode); Overload; Virtual;
		Procedure Process(Node:BreakStatementNode); Overload; Virtual;
		Procedure Process(Node:ContinueStatementNode); Overload; Virtual;
		Procedure Process(Node:AssignmentNode); Overload; Virtual;
		Procedure Process(Node:IfStatementNode); Overload; Virtual;
		Procedure Process(Node:ExpressionStatementNode); Overload; Virtual;
		Procedure Process(Node:CaseSelectorNode); Overload; Virtual;
		Procedure Process(Node:CaseStatementNode); Overload; Virtual;
		Procedure Process(Node:LoopStatementNode); Overload; Virtual;
		Procedure Process(Node:RepeatLoopNode); Overload; Virtual;
		Procedure Process(Node:WhileLoopNode); Overload; Virtual;
		Procedure Process(Node:ForLoopNode); Overload; Virtual;
		Procedure Process(Node:WithStatementNode); Overload; Virtual;
		Procedure Process(Node:TryFinallyStatementNode); Overload; Virtual;
		Procedure Process(Node:ExceptionBlockNode); Overload; Virtual;
		Procedure Process(Node:TryExceptStatementNode); Overload; Virtual;
		Procedure Process(Node:RaiseStatementNode); Overload; Virtual;
		Procedure Process(Node:OnStatementNode); Overload; Virtual;
		Procedure Process(Node:AssemblerBlockNode); Overload; Virtual;
		Procedure Process(Node:InterfaceTypeNode); Overload; Virtual;
		Procedure Process(Node:StructuredTypeNode); Overload; Virtual;
		Procedure Process(Node:RecordTypeNode); Overload; Virtual;
		Procedure Process(Node:RecordFieldDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ArrayTypeNode); Overload; Virtual;
		Procedure Process(Node:SetTypeNode); Overload; Virtual;
		Procedure Process(Node:FileTypeNode); Overload; Virtual;
		Procedure Process(Node:VariantDeclarationNode); Overload; Virtual;
		Procedure Process(Node:VarEntryDeclarationNode); Overload; Virtual;
		Procedure Process(Node:ExportItemNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedTypeNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedVariableTypeNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedIntegralTypeNode); Overload; Virtual;
		Procedure Process(Node:UnresolvedOrdinalTypeNode); Overload; Virtual;
	End;

Implementation

Uses TERRA_Error;

Procedure ASTProcessor.Dispatch(Node:ASTNode);
Begin
	If (Node Is UnresolvedOrdinalTypeNode) Then Process(UnresolvedOrdinalTypeNode(Node)) Else
	If (Node Is UnresolvedIntegralTypeNode) Then Process(UnresolvedIntegralTypeNode(Node)) Else
	If (Node Is UnresolvedVariableTypeNode) Then Process(UnresolvedVariableTypeNode(Node)) Else
	If (Node Is UnresolvedTypeNode) Then Process(UnresolvedTypeNode(Node)) Else
	If (Node Is UnresolvedNode) Then Process(UnresolvedNode(Node)) Else
	If (Node Is ExportItemNode) Then Process(ExportItemNode(Node)) Else
	If (Node Is VarEntryDeclarationNode) Then Process(VarEntryDeclarationNode(Node)) Else
	If (Node Is VariantDeclarationNode) Then Process(VariantDeclarationNode(Node)) Else
	If (Node Is FileTypeNode) Then Process(FileTypeNode(Node)) Else
	If (Node Is SetTypeNode) Then Process(SetTypeNode(Node)) Else
	If (Node Is ArrayTypeNode) Then Process(ArrayTypeNode(Node)) Else
	If (Node Is RecordFieldDeclarationNode) Then Process(RecordFieldDeclarationNode(Node)) Else
	If (Node Is RecordTypeNode) Then Process(RecordTypeNode(Node)) Else
	If (Node Is StructuredTypeNode) Then Process(StructuredTypeNode(Node)) Else
	If (Node Is InterfaceTypeNode) Then Process(InterfaceTypeNode(Node)) Else
	If (Node Is AssemblerBlockNode) Then Process(AssemblerBlockNode(Node)) Else
	If (Node Is OnStatementNode) Then Process(OnStatementNode(Node)) Else
	If (Node Is RaiseStatementNode) Then Process(RaiseStatementNode(Node)) Else
	If (Node Is TryExceptStatementNode) Then Process(TryExceptStatementNode(Node)) Else
	If (Node Is ExceptionBlockNode) Then Process(ExceptionBlockNode(Node)) Else
	If (Node Is TryFinallyStatementNode) Then Process(TryFinallyStatementNode(Node)) Else
	If (Node Is WithStatementNode) Then Process(WithStatementNode(Node)) Else
	If (Node Is ForLoopNode) Then Process(ForLoopNode(Node)) Else
	If (Node Is WhileLoopNode) Then Process(WhileLoopNode(Node)) Else
	If (Node Is RepeatLoopNode) Then Process(RepeatLoopNode(Node)) Else
	If (Node Is LoopStatementNode) Then Process(LoopStatementNode(Node)) Else
	If (Node Is CaseStatementNode) Then Process(CaseStatementNode(Node)) Else
	If (Node Is CaseSelectorNode) Then Process(CaseSelectorNode(Node)) Else
	If (Node Is ExpressionStatementNode) Then Process(ExpressionStatementNode(Node)) Else
	If (Node Is IfStatementNode) Then Process(IfStatementNode(Node)) Else
	If (Node Is AssignmentNode) Then Process(AssignmentNode(Node)) Else
	If (Node Is ContinueStatementNode) Then Process(ContinueStatementNode(Node)) Else
	If (Node Is BreakStatementNode) Then Process(BreakStatementNode(Node)) Else
	If (Node Is EmptyStatementNode) Then Process(EmptyStatementNode(Node)) Else
	If (Node Is GotoStatementNode) Then Process(GotoStatementNode(Node)) Else
	If (Node Is LabelStatementNode) Then Process(LabelStatementNode(Node)) Else
	If (Node Is ImportDirectivesNode) Then Process(ImportDirectivesNode(Node)) Else
	If (Node Is ExternalDirectiveNode) Then Process(ExternalDirectiveNode(Node)) Else
	If (Node Is InterfaceDeclarationNode) Then Process(InterfaceDeclarationNode(Node)) Else
	If (Node Is ClassDeclarationNode) Then Process(ClassDeclarationNode(Node)) Else
	If (Node Is CompositeDeclarationNode) Then Process(CompositeDeclarationNode(Node)) Else
	If (Node Is MethodDefinitionNode) Then Process(MethodDefinitionNode(Node)) Else
	If (Node Is RoutineDefinitionNode) Then Process(RoutineDefinitionNode(Node)) Else
	If (Node Is RoutineDeclarationNode) Then Process(RoutineDeclarationNode(Node)) Else
	If (Node Is EnumValueNode) Then Process(EnumValueNode(Node)) Else
	If (Node Is ConstDeclarationNode) Then Process(ConstDeclarationNode(Node)) Else
	If (Node Is UnresolvedCallNode) Then Process(UnresolvedCallNode(Node)) Else
	If (Node Is UnresolvedIdNode) Then Process(UnresolvedIdNode(Node)) Else
	If (Node Is UnresolvedLvalueNode) Then Process(UnresolvedLvalueNode(Node)) Else
	If (Node Is IdentifierStaticNode) Then Process(IdentifierStaticNode(Node)) Else
	If (Node Is IdentifierListNode) Then Process(IdentifierListNode(Node)) Else
	If (Node Is IdentifierNode) Then Process(IdentifierNode(Node)) Else
	If (Node Is ObjectAccessNode) Then Process(ObjectAccessNode(Node)) Else
	If (Node Is InheritedCallNode) Then Process(InheritedCallNode(Node)) Else
	If (Node Is RoutineCallNode) Then Process(RoutineCallNode(Node)) Else
	If (Node Is PointerDereferenceNode) Then Process(PointerDereferenceNode(Node)) Else
	If (Node Is ArrayAccessNode) Then Process(ArrayAccessNode(Node)) Else
	If (Node Is StaticCastNode) Then Process(StaticCastNode(Node)) Else
	If (Node Is ExprAsLvalueNode) Then Process(ExprAsLvalueNode(Node)) Else
	If (Node Is LvalueExpressionNode) Then Process(LvalueExpressionNode(Node)) Else
	If (Node Is LValueAsExprNode) Then Process(LValueAsExprNode(Node)) Else
	If (Node Is SetExpressionNode) Then Process(SetExpressionNode(Node)) Else
	If (Node Is AddressLvalueNode) Then Process(AddressLvalueNode(Node)) Else
	If (Node Is LogicalNotNode) Then Process(LogicalNotNode(Node)) Else
	If (Node Is UnaryMinusNode) Then Process(UnaryMinusNode(Node)) Else
	If (Node Is UnaryPlusNode) Then Process(UnaryPlusNode(Node)) Else
	If (Node Is SimpleUnaryExpressionNode) Then Process(SimpleUnaryExpressionNode(Node)) Else
	If (Node Is UnaryExpressionNode) Then Process(UnaryExpressionNode(Node)) Else
	If (Node Is RuntimeCastNode) Then Process(RuntimeCastNode(Node)) Else
	If (Node Is IsExpressionNode) Then Process(IsExpressionNode(Node)) Else
	If (Node Is TypeBinaryExpressionNode) Then Process(TypeBinaryExpressionNode(Node)) Else
	If (Node Is GreaterOrEqualNode) Then Process(GreaterOrEqualNode(Node)) Else
	If (Node Is GreaterThanNode) Then Process(GreaterThanNode(Node)) Else
	If (Node Is LessOrEqualNode) Then Process(LessOrEqualNode(Node)) Else
	If (Node Is LessThanNode) Then Process(LessThanNode(Node)) Else
	If (Node Is NotEqualNode) Then Process(NotEqualNode(Node)) Else
	If (Node Is EqualNode) Then Process(EqualNode(Node)) Else
	If (Node Is LogicalXorNode) Then Process(LogicalXorNode(Node)) Else
	If (Node Is LogicalOrNode) Then Process(LogicalOrNode(Node)) Else
	If (Node Is LogicalAndNode) Then Process(LogicalAndNode(Node)) Else
	If (Node Is ComparisonBinaryExpressionNode) Then Process(ComparisonBinaryExpressionNode(Node)) Else
	If (Node Is LogicalBinaryExpressionNode) Then Process(LogicalBinaryExpressionNode(Node)) Else
	If (Node Is ShiftLeftNode) Then Process(ShiftLeftNode(Node)) Else
	If (Node Is ShiftRightNode) Then Process(ShiftRightNode(Node)) Else
	If (Node Is ModulusNode) Then Process(ModulusNode(Node)) Else
	If (Node Is QuotientNode) Then Process(QuotientNode(Node)) Else
	If (Node Is DivisionNode) Then Process(DivisionNode(Node)) Else
	If (Node Is ProductNode) Then Process(ProductNode(Node)) Else
	If (Node Is AdditionNode) Then Process(AdditionNode(Node)) Else
	If (Node Is SubtractionNode) Then Process(SubtractionNode(Node)) Else
	If (Node Is SetRangeNode) Then Process(SetRangeNode(Node)) Else
	If (Node Is InExpressionNode) Then Process(InExpressionNode(Node)) Else
	If (Node Is BinaryExpressionNode) Then Process(BinaryExpressionNode(Node)) Else
	If (Node Is NullLiteralNode) Then Process(NullLiteralNode(Node)) Else
	If (Node Is RealLiteralNode) Then Process(RealLiteralNode(Node)) Else
	If (Node Is StringLiteralNode) Then Process(StringLiteralNode(Node)) Else
	If (Node Is BoolLiteralNode) Then Process(BoolLiteralNode(Node)) Else
	If (Node Is CharLiteralNode) Then Process(CharLiteralNode(Node)) Else
	If (Node Is IntLiteralNode) Then Process(IntLiteralNode(Node)) Else
	If (Node Is OrdinalLiteralNode) Then Process(OrdinalLiteralNode(Node)) Else
	If (Node Is LiteralNode) Then Process(LiteralNode(Node)) Else
	If (Node Is MetaClassTypeNode) Then Process(MetaClassTypeNode(Node)) Else
	If (Node Is ClassRefTypeNode) Then Process(ClassRefTypeNode(Node)) Else
	If (Node Is ClassTypeNode) Then Process(ClassTypeNode(Node)) Else
	If (Node Is ArrayPropertyNode) Then Process(ArrayPropertyNode(Node)) Else
	If (Node Is ObjectSectionNode) Then Process(ObjectSectionNode(Node)) Else
	If (Node Is MethodDeclarationNode) Then Process(MethodDeclarationNode(Node)) Else
	If (Node Is MethodTypeNode) Then Process(MethodTypeNode(Node)) Else
	If (Node Is ProceduralTypeNode) Then Process(ProceduralTypeNode(Node)) Else
	If (Node Is PropertyDeclarationNode) Then Process(PropertyDeclarationNode(Node)) Else
	If (Node Is FieldDeclarationNode) Then Process(FieldDeclarationNode(Node)) Else
	If (Node Is CompositeTypeNode) Then Process(CompositeTypeNode(Node)) Else
	If (Node Is PropertySpecifiersNode) Then Process(PropertySpecifiersNode(Node)) Else
	If (Node Is PointerTypeNode) Then Process(PointerTypeNode(Node)) Else
	If (Node Is VariantTypeNode) Then Process(VariantTypeNode(Node)) Else
	If (Node Is EnumTypeNode) Then Process(EnumTypeNode(Node)) Else
	If (Node Is EnumValueListNode) Then Process(EnumValueListNode(Node)) Else
	If (Node Is RangeTypeNode) Then Process(RangeTypeNode(Node)) Else
	If (Node Is FixedStringTypeNode) Then Process(FixedStringTypeNode(Node)) Else
	If (Node Is StringTypeNode) Then Process(StringTypeNode(Node)) Else
	If (Node Is CurrencyTypeNode) Then Process(CurrencyTypeNode(Node)) Else
	If (Node Is ExtendedTypeNode) Then Process(ExtendedTypeNode(Node)) Else
	If (Node Is DoubleTypeNode) Then Process(DoubleTypeNode(Node)) Else
	If (Node Is FloatTypeNode) Then Process(FloatTypeNode(Node)) Else
	If (Node Is RealTypeNode) Then Process(RealTypeNode(Node)) Else
	If (Node Is CharTypeNode) Then Process(CharTypeNode(Node)) Else
	If (Node Is BoolTypeNode) Then Process(BoolTypeNode(Node)) Else
	If (Node Is UnsignedIntegerTypeNode) Then Process(UnsignedIntegerTypeNode(Node)) Else
	If (Node Is SignedIntegerTypeNode) Then Process(SignedIntegerTypeNode(Node)) Else
	If (Node Is IntegerTypeNode) Then Process(IntegerTypeNode(Node)) Else
	If (Node Is IntegralTypeNode) Then Process(IntegralTypeNode(Node)) Else
	If (Node Is ScalarTypeNode) Then Process(ScalarTypeNode(Node)) Else
	If (Node Is ConstIdentifierNode) Then Process(ConstIdentifierNode(Node)) Else
	If (Node Is RecordConstNode) Then Process(RecordConstNode(Node)) Else
	If (Node Is FieldInitListNode) Then Process(FieldInitListNode(Node)) Else
	If (Node Is FieldInitNode) Then Process(FieldInitNode(Node)) Else
	If (Node Is ArrayConstNode) Then Process(ArrayConstNode(Node)) Else
	If (Node Is StructuredConstantNode) Then Process(StructuredConstantNode(Node)) Else
	If (Node Is PackageNode) Then Process(PackageNode(Node)) Else
	If (Node Is UnitNode) Then Process(UnitNode(Node)) Else
	If (Node Is LibraryNode) Then Process(LibraryNode(Node)) Else
	If (Node Is RoutineSectionNode) Then Process(RoutineSectionNode(Node)) Else
	If (Node Is CallableDeclarationNode) Then Process(CallableDeclarationNode(Node)) Else
	If (Node Is FunctionAttributeNode) Then Process(FunctionAttributeNode(Node)) Else
	If (Node Is ParametersSectionNode) Then Process(ParametersSectionNode(Node)) Else
	If (Node Is OutParamDeclarationNode) Then Process(OutParamDeclarationNode(Node)) Else
	If (Node Is ConstParamDeclarationNode) Then Process(ConstParamDeclarationNode(Node)) Else
	If (Node Is VarParamDeclarationNode) Then Process(VarParamDeclarationNode(Node)) Else
	If (Node Is ParamDeclarationNode) Then Process(ParamDeclarationNode(Node)) Else
	If (Node Is VarDeclarationNode) Then Process(VarDeclarationNode(Node)) Else
	If (Node Is ValueDeclarationNode) Then Process(ValueDeclarationNode(Node)) Else
	If (Node Is LabelDeclarationNode) Then Process(LabelDeclarationNode(Node)) Else
	If (Node Is ConstExpressionNode) Then Process(ConstExpressionNode(Node)) Else
	If (Node Is ExpressionListNode) Then Process(ExpressionListNode(Node)) Else
	If (Node Is ExpressionNode) Then Process(ExpressionNode(Node)) Else
	If (Node Is ConstantValueNode) Then Process(ConstantValueNode(Node)) Else
	If (Node Is ProgramNode) Then Process(ProgramNode(Node)) Else
	If (Node Is ProgramSectionNode) Then Process(ProgramSectionNode(Node)) Else
	If (Node Is BlockStatementNode) Then Process(BlockStatementNode(Node)) Else
	If (Node Is StatementListNode) Then Process(StatementListNode(Node)) Else
	If (Node Is ImplementationSectionNode) Then Process(ImplementationSectionNode(Node)) Else
	If (Node Is InterfaceSectionNode) Then Process(InterfaceSectionNode(Node)) Else
	If (Node Is TopLevelDeclarationSectionNode) Then Process(TopLevelDeclarationSectionNode(Node)) Else
	If (Node Is SectionNode) Then Process(SectionNode(Node)) Else
	If (Node Is StatementNode) Then Process(StatementNode(Node)) Else
	If (Node Is UnitListNode) Then Process(UnitListNode(Node)) Else
	If (Node Is UnitItemNode) Then Process(UnitItemNode(Node)) Else
	If (Node Is DeclarationListNode) Then Process(DeclarationListNode(Node)) Else
	If (Node Is DeclarationNode) Then Process(DeclarationNode(Node)) Else
	If (Node Is MetaTypeNode) Then Process(MetaTypeNode(Node)) Else
	If (Node Is TypeListNode) Then Process(TypeListNode(Node)) Else
	If (Node Is TypeNode) Then Process(TypeNode(Node)) Else
	If (Node Is SourceNode) Then Process(SourceNode(Node)) Else
	If (Node Is ListNode) Then Process(ListNode(Node)) Else

	RaiseError('Cannot dispatch '+Node.ClassName);
End;

Procedure ASTProcessor.Process(Node:ListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SourceNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TypeListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:MetaTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:DeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:DeclarationListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnitItemNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnitListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TopLevelDeclarationSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:InterfaceSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ImplementationSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StatementListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:BlockStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ProgramSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ProgramNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ConstantValueNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExpressionListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ConstExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LabelDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ValueDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:VarDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ParamDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:VarParamDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ConstParamDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:OutParamDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ParametersSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FunctionAttributeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CallableDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RoutineSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LibraryNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnitNode);
Begin
End;

Procedure ASTProcessor.Process(Node:PackageNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StructuredConstantNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ArrayConstNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FieldInitNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FieldInitListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RecordConstNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ConstIdentifierNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ScalarTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IntegralTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IntegerTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SignedIntegerTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnsignedIntegerTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:BoolTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CharTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RealTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FloatTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:DoubleTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExtendedTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CurrencyTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StringTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FixedStringTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RangeTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:EnumValueListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:EnumTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:VariantTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:PointerTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:PropertySpecifiersNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CompositeTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FieldDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:PropertyDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ProceduralTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:MethodTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:MethodDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ObjectSectionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ArrayPropertyNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ClassTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ClassRefTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:MetaClassTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:OrdinalLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IntLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CharLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:BoolLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StringLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RealLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:NullLiteralNode);
Begin
End;

Procedure ASTProcessor.Process(Node:BinaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:InExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SetRangeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SubtractionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:AdditionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ProductNode);
Begin
End;

Procedure ASTProcessor.Process(Node:DivisionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:QuotientNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ModulusNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ShiftRightNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ShiftLeftNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LogicalBinaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ComparisonBinaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LogicalAndNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LogicalOrNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LogicalXorNode);
Begin
End;

Procedure ASTProcessor.Process(Node:EqualNode);
Begin
End;

Procedure ASTProcessor.Process(Node:NotEqualNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LessThanNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LessOrEqualNode);
Begin
End;

Procedure ASTProcessor.Process(Node:GreaterThanNode);
Begin
End;

Procedure ASTProcessor.Process(Node:GreaterOrEqualNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TypeBinaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IsExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RuntimeCastNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SimpleUnaryExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnaryPlusNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnaryMinusNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LogicalNotNode);
Begin
End;

Procedure ASTProcessor.Process(Node:AddressLvalueNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SetExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LValueAsExprNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LvalueExpressionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExprAsLvalueNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StaticCastNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ArrayAccessNode);
Begin
End;

Procedure ASTProcessor.Process(Node:PointerDereferenceNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RoutineCallNode);
Begin
End;

Procedure ASTProcessor.Process(Node:InheritedCallNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ObjectAccessNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IdentifierNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IdentifierListNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IdentifierStaticNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedLvalueNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedIdNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedCallNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ConstDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:EnumValueNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RoutineDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RoutineDefinitionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:MethodDefinitionNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CompositeDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ClassDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:InterfaceDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExternalDirectiveNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ImportDirectivesNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LabelStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:GotoStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:EmptyStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:BreakStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ContinueStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:AssignmentNode);
Begin
End;

Procedure ASTProcessor.Process(Node:IfStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExpressionStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CaseSelectorNode);
Begin
End;

Procedure ASTProcessor.Process(Node:CaseStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:LoopStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RepeatLoopNode);
Begin
End;

Procedure ASTProcessor.Process(Node:WhileLoopNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ForLoopNode);
Begin
End;

Procedure ASTProcessor.Process(Node:WithStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TryFinallyStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExceptionBlockNode);
Begin
End;

Procedure ASTProcessor.Process(Node:TryExceptStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RaiseStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:OnStatementNode);
Begin
End;

Procedure ASTProcessor.Process(Node:AssemblerBlockNode);
Begin
End;

Procedure ASTProcessor.Process(Node:InterfaceTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:StructuredTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RecordTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:RecordFieldDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ArrayTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:SetTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:FileTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:VariantDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:VarEntryDeclarationNode);
Begin
End;

Procedure ASTProcessor.Process(Node:ExportItemNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedVariableTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedIntegralTypeNode);
Begin
End;

Procedure ASTProcessor.Process(Node:UnresolvedOrdinalTypeNode);
Begin
End;



End.
