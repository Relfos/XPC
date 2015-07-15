Program test21;

Var
  A, B, C, D:Integer;

Function Lol(X,Y:Integer):Integer;
Begin
  A := 2;
End;

Function Bob(U:Boolean; V:Integer):Boolean;
Begin
  A := 2;
End;

begin
  A := Lol(2);
  B := Bob(True, 3);
end.
