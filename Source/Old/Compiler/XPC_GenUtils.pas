{
  XPC_GenUtils.pas
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
//callconv
Unit XPC_GenUtils;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_IO, TERRA_FileIO;

Const
  LicenseFile = 'Grammar\license.txt';

Function CreateXPCFile(FileName:AnsiString):Stream;

Implementation

Var
  License:AnsiString;

Function CreateXPCFile(FileName:AnsiString):Stream;
Begin
  Result := FileStream.Create(FileName);
  If (License<>'') Then
  Begin
    Result.WriteLine('{');
    Result.WriteLine(#9+FileName);
    Result.WriteLine(License);
    Result.WriteLine('}');
    Result.WriteLine();
  End;
  Result.WriteLine('//Auto-generated, do not edit manually');
End;


Var
  Src:Stream;

Initialization
  License := '';

  If FileStream.Exists(LicenseFile) Then
  Begin
    Src := MemoryStream.Create(LicenseFile);
    Src.ReadLines(License);
    Src.Destroy();
  End;
End.