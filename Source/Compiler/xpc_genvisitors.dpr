Program xpc_genvisitors;

Uses TERRA_Utils, TERRA_FileUtils, TERRA_IO, TERRA_FileIO
//, XPC_ASTVisitor
;

Const
  TargetClass = 'ASTProcessor';
  MethodName = 'Process';
  
  LicenseFile = 'Grammar\license.txt';

Var
  Src, Dest:Stream;

  TargetName, Code, Name, License:AnsiString;

  I,J,K:Integer;
Begin
  TargetName := 'XPC_'+TargetClass+'.pas';
  
  If FileStream.Exists(LicenseFile) Then
  Begin
    Src := MemoryStream.Create(LicenseFile);
    Src.ReadLines(License);
    Src.Destroy();
  End;

  Src := MemoryStream.Create('XPC_ASTNodes.pas');
  Src.ReadLines(Code);
  Src.Destroy();

  Dest := FileStream.Create(TargetName);
  Dest.WriteLine('{');
  Dest.WriteLine(#9+TargetName);
  Dest.WriteLine(License);
  Dest.WriteLine('}');
  Dest.WriteLine();
  Dest.WriteLine('//Auto-generated, do not edit manually');
  Dest.WriteLine('Unit '+GetFileName(TargetName, True)+';');
  Dest.WriteLine();
  Dest.WriteLine('Interface');
  Dest.WriteLine();
  Dest.WriteLine('Uses XPC_ASTNodes;');
  Dest.WriteLine();
  Dest.WriteLine('Type');
  Dest.WriteLine(#9'ASTProcessor = Class');



  I := PosRev('Implementation', Code);
  Code := Copy(Code, 1, Pred(I));

  Repeat
    I := Pos('Node = Class', Code);

    If I<=0 Then
      Break;

    K := 0;
    For J:=I DownTo 1 Do
    If (Code[J]=' ') Or (Code[J]=#10) Or (Code[J]=#13) Then
    Begin
      K := J;
      Break;
    End;

    Name := TrimLeft(TrimRight(Copy(Code, K, I-K + 4)));

    Code := Copy(Code, I+10, MaxInt);

    If Name = 'InterfaceSectionNode' Then
      IntToString(2);

    Dest.WriteLine(#9#9'Procedure '+MethodName+'(Node:'+Name+'); Overload; Virtual; Abstract;');
  Until False;

  Dest.WriteLine(#9'End;');
  Dest.WriteLine();
  Dest.WriteLine('Implementation');
  Dest.WriteLine();
  Dest.WriteLine('End.');
  
End.