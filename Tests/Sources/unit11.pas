Unit unit1;

Interface

Type
  Animal = Class
    Namet:String;
    Legs:Integer;

    Constructor Create;
    Destructor Destroy;
    Procedure Eat(Food:Integer); Virtual; Abstract;
    Class Function GetType:String;
  End;

Implementation

{ Animal }
Constructor Animal.Create;
Begin
  Self.Legs := 0;
End;

Destructor Animal.Destroy;
Begin

End;

Class function Animal.GetType: String;
Begin
  Result := 'type';
End;

Initialization
  WriteLn('Started!');
Finalization
  WriteLn('Finished!');
End.
