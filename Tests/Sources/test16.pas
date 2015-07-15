Program test0;

Var
  A, B, C, D:Integer;
begin
  A := 1;
  For B:=2 To A + 3 Do
    C := B + A;

  A := B + C;
end.
