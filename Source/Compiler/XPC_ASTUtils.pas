{
  XPC_ASTUtils.pas
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

Unit XPC_ASTUtils;

Interface
Uses TERRA_Collections, XPC_ASTNodes;

Function MakeThreadVars(Src:DeclarationListNode):DeclarationListNode;
Function CreateDecls(Ids:IdentifierListNode; VarTypes:TypeNode; Init:ExpressionNode; Const AbsoluteID:ASTString = ''):DeclarationListNode;
Function CreateLabelDecls(Ids:IdentifierListNode):DeclarationListNode;
Function CreateParamDecls(Ids:IdentifierListNode; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum):DeclarationListNode;
Function CreateVarParamDecls(Ids:IdentifierListNode; ParamType:TypeNode):DeclarationListNode;
Function CreateConstParamDecls(Ids:IdentifierListNode; ParamType:TypeNode; init:ExpressionNode = Nil):DeclarationListNode;
Function CreateFieldDecls(Ids:IdentifierListNode; FieldsType:TypeNode):DeclarationListNode;
Function JoinImportDirectives(d1, d2:FunctionDirectiveListNode; i:FunctionAttributeNode):ImportDirectivesNode; Overload;
Function JoinImportDirectives(d1, d2:FunctionDirectiveListNode; e:ExternalDirectiveNode):ImportDirectivesNode; Overload;
Function CreateRecordUnionField(Src:ExpressionListNode; Fields:DeclarationListNode):DeclarationListNode;
Procedure MakeFieldDeclarationsStatic(DeclList:DeclarationListNode);
Function CheckDirectiveId(expected:AnsiString; Const idtoken:ASTString):Boolean;

Implementation
Uses TERRA_Error;

Function MakeThreadVars(Src:DeclarationListNode):DeclarationListNode;
Var
  I:Integer;
  VarDecl:VarDeclarationNode;
Begin
  For I:=0 To Pred(Src.Count) Do
  Begin
    VarDecl := VarDeclarationNode(Src.Get(I));
    VarDecl.IsThrVar := True;
  End;

  Result := Src;
End;

Function CreateDecls(Ids:IdentifierListNode; VarTypes:TypeNode; Init:ExpressionNode; Const AbsoluteID:ASTString = ''):DeclarationListNode;
Var
  I:Integer;
  V:VarDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    V := VarDeclarationNode.Create(Ids.GetIdentifier(I).Name, VarTypes, AbsoluteID, Init);
    Result.Add(V);
  End;
End;

Function CreateLabelDecls(Ids:IdentifierListNode):DeclarationListNode;
Var
  I:Integer;
  L:LabelDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    L := LabelDeclarationNode.Create(Ids.GetIdentifier(I).Name, Nil);
    Result.Add(L);
  End;
End;

Function CreateParamDecls(Ids:IdentifierListNode; ParamType:TypeNode; Init:ExpressionNode; Kind:ParameterKindEnum):DeclarationListNode;
Var
  I:Integer;
  P:ParamDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    P := ParamDeclarationNode.Create(Ids.GetIdentifier(I).Name, ParamType, Init, Kind);
    Result.Add(P);
  End;
End;

Function CreateVarParamDecls(Ids:IdentifierListNode; ParamType:TypeNode):DeclarationListNode;
Var
  I:Integer;
  P:ParamDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    P := VarParamDeclarationNode.Create(Ids.GetIdentifier(I).Name, ParamType);
    Result.Add(P);
  End;
End;

Function CreateConstParamDecls(Ids:IdentifierListNode; ParamType:TypeNode; init:ExpressionNode = Nil):DeclarationListNode;
Var
  I:Integer;
  P:ParamDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    P := ConstParamDeclarationNode.Create(Ids.GetIdentifier(I).Name, ParamType, Init);
    Result.Add(P);
  End;
End;

Function CreateFieldDecls(Ids:IdentifierListNode; FieldsType:TypeNode):DeclarationListNode;
Var
  I:Integer;
  P:FieldDeclarationNode;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Ids.Count) Do
  Begin
    P := FieldDeclarationNode.Create(Ids.GetIdentifier(I).Name, FieldsType);
    Result.Add(P);
  End;
End;

Function JoinImportDirectives(d1, d2:FunctionDirectiveListNode; i:FunctionAttributeNode):ImportDirectivesNode; Overload;
Begin
  Result := ImportDirectivesNode.Create(i);
  Result.Add(d1);
	Result.Add(d2);
End;

Function JoinImportDirectives(d1, d2:FunctionDirectiveListNode; e:ExternalDirectiveNode):ImportDirectivesNode; Overload;
Begin
  Result := JoinImportDirectives(d1, d2, FunctionAttributeNode.Create(attribute_External));
  Result._External := e;
End;

Function CreateRecordUnionField(Src:ExpressionListNode; Fields:DeclarationListNode):DeclarationListNode;
Var
  I:Integer;
Begin
  Result := DeclarationListNode.Create();
  For I:=0 To Pred(Src.Count) Do
  Begin
    Result.Add(VarEntryDeclarationNode.Create(Src.GetExpression(I), Fields));
  End;
End;

Procedure MakeFieldDeclarationsStatic(DeclList:DeclarationListNode);
Var
  I:Integer;
Begin
  For I:=0 To Pred(DeclList.Count) Do
    FieldDeclarationNode(DeclList.Get(I)).IsStatic := True;
End;

Function CheckDirectiveId(expected:AnsiString; Const idtoken:ASTString):Boolean;
Begin
  If (expected <> idtoken) Then
  Begin
    RaiseError('Invalid directive ' + idtoken + ', expected: ' + expected);
    Result := False;
  End Else
    Result := True;
End;

End.