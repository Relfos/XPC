Program test0;

Var
  A, B, C, D:Integer;
begin
  A := 2;
  B := 4;
  While (A<B) Do
    C := 2;

  While (A<B) Do
  Begin
    A := A - 1;
    C := 2;
  End;

end.
