program test33;

type
    TSuit = (Hearts=13, Diamonds, Clubs=22, Spades);
var
   suit : TSuit;
   a:Boolean;

   x:Integer;
begin
   suit := Clubs;

   if (x in 23..45) then
    a := false;

   if (hearts in clubs) then
    a := True;
    
end.
