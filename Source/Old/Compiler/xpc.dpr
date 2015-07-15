{
	XPC_PascalLexer.pas
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

Program XPC;

{$APPTYPE CONSOLE}

Uses SysUtils, TERRA_Utils, TERRA_IO, TERRA_FileIO,
  XPC_PascalLexer, XPC_PascalParser, XPC_ASTNodes, XPC_ASTPrinter;

Const
  SrcName = 'Tests\Sources\test1.dpr';

Var
  Src:Stream;
  Parser:PascalParser;
  Result:ASTNode;
  Printer:ASTPrinter;
Begin
  WriteLn('Compiling ', SrcName);
  Try
    Src := MemoryStream.Create(SrcName);
    Parser := PascalParser.Create(Src);

    Result := Parser.Parse();

  Except
    On E : Exception Do
    Begin
      WriteLn(E.Message);

      If Assigned(Parser) Then
        Parser.Destroy();

      If Assigned(Src) Then
        Src.Destroy();
    End;
  End;

  If Assigned(Result) Then
  Begin
    Printer := ASTPrinter.Create();
    Printer.Dispatch(Result);
    Printer.Destroy();
    WriteLn('Compile sucess!');
  End;

  ReadLn;
End.
