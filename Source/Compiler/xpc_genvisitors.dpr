Program xpc_genvisitors;

Uses TERRA_Utils, TERRA_OS, TERRA_FileUtils, TERRA_IO, TERRA_FileIO, XPC_GenUtils;

Const
  TargetClass = 'ASTProcessor';
  MethodName = 'Process';

Type
  ClassRec = Class
    Name:AnsiString;
    Parent:ClassRec;
    PropImpl:AnsiString;
  End;

Var
  Classes:Array Of ClassRec;
  ClassCount:Integer;

Function FindParent(Const Name:AnsiString):ClassRec;
Var
  I:Integer;
Begin
  Result := Nil;

  For I:=0 To Pred(ClassCount) Do
  If (LowStr(Classes[I].Name) = LowStr(Name)) Then
  Begin
    Result := Classes[I];
  End;
End;

Var
  Src, ProcessorFile, PrinterFile, Temp:Stream;

  TargetName, Code, Name:AnsiString;
  S, S2, S3, S4, Impl:AnsiString;

  PrinterDecl, PrinterImpl, PropImpl, DispImpl:AnsiString;

  I,J,K:Integer;

  Current:ClassRec;

Begin
  ClassCount := 0;

  TargetName := 'XPC_'+TargetClass+'.pas';

  Src := MemoryStream.Create('XPC_ASTNodes.pas');
  Src.ReadLines(Code);
  Src.Destroy();

  ProcessorFile := CreateXPCFile(TargetName);
  ProcessorFile.WriteLine('Unit '+GetFileName(TargetName, True)+';');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Interface');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Uses XPC_ASTNodes;');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Type');
  ProcessorFile.WriteLine(#9''+TargetClass+' = Class');
  ProcessorFile.WriteLine(#9#9+'Procedure Dispatch(Node:ASTNode);');

  PrinterFile := CreateXPCFile('XPC_ASTPrinter.pas');
  PrinterFile.WriteLine('Unit XPC_ASTPrinter;');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Interface');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Uses TERRA_Utils, XPC_ASTNodes, XPC_'+TargetClass+';');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Type');
  PrinterFile.WriteLine(#9+'ASTPrinter = Class('+TargetClass+')');
  PrinterFile.WriteLine(#9#9+'Procedure Print(Const S:AnsiString); Virtual;');
  PrinterFile.WriteLine(#9#9+'Procedure PrintNode(Node:ASTNode);');

  I := PosRev('Implementation'+CrLf, Code);
  Code := Copy(Code, 1, Pred(I));

  Repeat
    S2 := 'Node = Class(';
    I := Pos(S2, Code);

    If I<=0 Then
      Break;


    S3 := Copy(Code, (I+Length(S2)), MaxInt);
    J := Pos(')', S3);
    S3 := Copy(S3, 1, Pred(J));

    K := 0;
    For J:=I DownTo 1 Do
    If (Code[J]=' ') Or (Code[J]=#10) Or (Code[J]=#13) Then
    Begin
      K := J;
      Break;
    End;

    Name := TrimLeft(TrimRight(Copy(Code, K, I-K + 4)));

    Current := ClassRec.Create;
    Inc(ClassCount);
    SetLength(Classes, ClassCount);
    Classes[Pred(ClassCount)] := Current;

    Current.Name := Name;
    Current.Parent := FindParent(S3);


    Code := Copy(Code, I+10, MaxInt);

    If Name = 'InterfaceSectionNode' Then
      IntToString(2);

    S := MethodName+'(Node:'+Name+');';
    ProcessorFile.WriteLine(#9#9+'Procedure '+S+' Overload; Virtual;');

    J := Pos('End;', Code);
    S2 := Copy(Code, 1, Pred(J));


    If Assigned(Current.Parent) Then
      PropImpl := Current.Parent.PropImpl
    Else
      PropImpl := '';

    J := Pos('Published'+CrLf, S2);
    If J>0 Then
    Begin
      S2 := Copy(S2, J + Length('Published'), MaxInt);
      Temp := MemoryStream.Create(Length(S2), @S2[1]);
      While Not Temp.EOF Do
      Begin
        Temp.ReadLine(S3);

        J := Pos('//', S3);
        If J>0 Then
          S3 := Copy(S3, 1, Pred(J));

        J := Pos(':', S3);
        If J>0 Then
        Begin
          S4 := Copy(S3, 1, Pred(J));
          S4 := TrimLeft(TrimRight(S4));
          PropImpl := PropImpl + #9+'PrintNode(Node.'+S4+');'+CrLf;
        End;
      End;
    End;

    Current.PropImpl := PropImpl;


    PrinterDecl := PrinterDecl + #9#9+'Procedure Process(Node:'+Name+'); Override;' +CrLf;
    PrinterImpl := PrinterImpl + 'Procedure ASTPrinter.Process(Node:'+Name+');'+CrLf ;

    If Pos('StatementListNode', Name)>0 Then
    Begin
      PrinterImpl := PrinterImpl + 'Var I:Integer;'+CrLf+'Begin'+CrLf+#9+'If Assigned(Node.List) Then'+CrLf+#9#9+'For I:=0 To Pred(Node.List.Count) Do'+CrLf+#9#9#9+'Dispatch(Node.List.Elements[I]);'+CrLf+'End;'+CrLf+CrLf;
    End Else
    If Pos('ListNode', Name)>0 Then
    Begin
      PrinterImpl := PrinterImpl + 'Var I:Integer;'+CrLf+'Begin'+CrLf+#9+'For I:=0 To Pred(Node.Count) Do'+CrLf+#9#9+'Dispatch(Node.Elements[I]);'+CrLf+'End;'+CrLf+CrLf;
    End Else
      PrinterImpl := PrinterImpl + 'Begin'+CrLf+PropImpl+'End;'+CrLf+CrLf;

    Impl := Impl + 'Procedure '+TargetClass+'.'+S+CrLf+'Begin'+CrLf+'End;'+CrLf+CrLf;


    DispImpl := #9+'If (Node Is '+Name+') Then Process('+Name+'(Node)) Else'+CrLf+DispImpl;
  Until False;

  PrinterFile.WriteLine(PrinterDecl);
  PrinterFile.WriteLine(#9+'End;');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Implementation');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Uses TERRA_OS;');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Procedure ASTPrinter.Print(Const S:AnsiString); ');
  PrinterFile.WriteLine('Begin');
  PrinterFile.WriteLine(#9'Write(S);');
  PrinterFile.WriteLine('End;');
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('Procedure ASTPrinter.PrintNode(Node:ASTNode);');
  PrinterFile.WriteLine('Begin');
  PrinterFile.WriteLine(#9'If (Node = Nil) Then');
  PrinterFile.WriteLine(#9#9'Exit;');
  PrinterFile.WriteLine(#9'Print(Spaces(Node.Level)+Node.ClassName+CrLf);');
  PrinterFile.WriteLine(#9'Dispatch(Node);');
  PrinterFile.WriteLine('End;');
  PrinterFile.WriteLine();

  PrinterFile.WriteLine(PrinterImpl);
  PrinterFile.WriteLine();
  PrinterFile.WriteLine('End.');
  PrinterFile.Destroy();

  ProcessorFile.WriteLine(#9'End;');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Implementation');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Uses TERRA_Error;');
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('Procedure '+TargetClass+'.Dispatch(Node:ASTNode);');
  ProcessorFile.WriteLine('Begin');
  ProcessorFile.WriteLine(DispImpl);
  ProcessorFile.WriteLine(#9+'RaiseError(''Cannot dispatch ''+Node.ClassName'+');');
  ProcessorFile.WriteLine('End;');
  ProcessorFile.WriteLine();

  ProcessorFile.WriteLine(Impl);
  ProcessorFile.WriteLine();
  ProcessorFile.WriteLine('End.');

End.