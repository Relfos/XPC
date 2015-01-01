Program XPC;

{$APPTYPE CONSOLE}

Uses XPC_PascalLexer, XPC_PascalParser, TERRA_IO, TERRA_FileIO;

Const
  SrcName = 'Tests\Sources\test1.dpr';

Var
  Src:Stream;
  Parser:PascalParser;
  Result:Integer;
begin
  WriteLn('Compiling ', SrcName);
  Src := MemoryStream.Create(SrcName);
  Parser := PascalParser.Create(Src);
  Try
    Result := Parser.Parse();
  Finally
    Parser.Destroy();
  End;
  Src.Destroy;

  If Result = 0 Then
    WriteLn('Compile sucess!');
  ReadLn;
end.
