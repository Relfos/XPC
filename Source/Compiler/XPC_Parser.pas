{
  XPC_Parser.pas
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


{$I-}
Unit XPC_Parser;

Interface

Uses XPC_ASTNodes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

Const
  // default stack size of parser
  yymaxdepth = 1024;

Type
  CustomParser = class
  protected
    yychar: integer; // current lookahead character
    yynerrs: integer;
    yyerrflag: integer;
    // Flags used internally by the parser routine:
    yyflag: (yyfnone, yyfaccept, yyfabort, yyferror);

    yystate, yysp, yyn : Integer;

    _Root:SourceNode;

    Procedure yyaction ( yyruleno : Integer ); Virtual; Abstract;

    Procedure yyerror(msg: string);
    // error message printing routine used by the parser 

    Procedure yyclearin;
    // delete the current lookahead token 

    Procedure yyaccept;
    // trigger accept action of the parser; yyparse accepts returning 0, as if it reached end of input 

    Procedure yyabort;
    // like yyaccept, but causes parser to return with value 1, as if an unrecoverable syntax error had been encountered 

    Procedure yyerrlab;
    // causes error recovery to be started, as if a syntax error had been encountered 

    Procedure yyerrok;
    // when in error mode, resets the parser to its normal mode of  operation

  Public
    Function Parse():ASTNode; Virtual; Abstract;
  End;

Implementation

Procedure CustomParser.yyerror(msg: string);
Begin
  writeln(msg);
End;

Procedure CustomParser.yyclearin;
Begin
  yychar := -1;
End;

Procedure CustomParser.yyaccept;
Begin
  yyflag := yyfaccept;
End;

Procedure CustomParser.yyabort;
Begin
  yyflag := yyfabort;
End;

Procedure CustomParser.yyerrlab;
Begin
  yyflag := yyferror;
End;

Procedure CustomParser.yyerrok;
Begin
  yyerrflag := 0;
End;

End.
