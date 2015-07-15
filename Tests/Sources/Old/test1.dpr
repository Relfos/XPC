program test1;

var
  public : real;
  a,b:string;
  a1,b1:string;
  a2,b2:string;
  a3,b3:string;

const
  x = 'test';
  x1 = 'test1';
  x2 = 'test2';
  x3 = 'test3';

function str1():string;
begin
  result := x;
end;

var
  c:string;
  c1:string;
  c2:string;
  c3:string;

begin
  a := 'a';
  b := str1();
  c := a+b;
  x1 := 2.5;
end.
