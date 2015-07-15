program parse_test_suite;

{$APPTYPE CONSOLE}

uses
  SysUtils, TERRA_String, TERRA_Utils, TERRA_Collections,
  TERRA_FileSearch, TERRA_FileStream, TERRA_Stream, TERRA_MemoryStream,
  TERRA_Parser,
  Delphi;

Var
  FileList:List;
  Info:FileInfo;
  It:Iterator;
  Parser:DelphiParser;
  Source:Stream;
  Total, Failures:Integer;
begin
  FileList := SearchFiles('tests', '*.pas', False);
  It := FileList.GetIterator();

  Total := 0;
  Failures := 0;
  Parser := DelphiParser.Create();
  While It.HasNext() Do
  Begin
    Info := FileInfo(It.Value);

    WriteLn(Info.Name);
    Source := MemoryStream.Create(Info.FullPath);

    Inc(Total);
    Try
      Parser.Parse(Source, True);

      WriteLn('Ok :',Info.Name);
    Except

    On E:ParsingException Do
    Begin
      WriteLn('Failed :',Info.Name);
      Inc(Failures);
    End;

    End;

    ReleaseObject(Source);
  End;
  ReleaseObject(It);
  ReleaseObject(Parser);

  WriteLn('Finished!');
  WriteLn('Failed ',Failures,' out of ',Total);
end.
