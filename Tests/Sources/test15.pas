Program test0;

Var
  A, B, C, D:Integer;
begin
  A := 1;
  Repeat
    B := A + 1;
    A := A + 2;
  Until A > 2;

  C := B + A;
end.
