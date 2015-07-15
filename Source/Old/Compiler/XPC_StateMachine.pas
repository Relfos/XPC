Unit XPC_StateMachine;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

Interface
Uses TERRA_IO;

Const
  max_chars = 2048;
  MaxStates = 128;

Type
  StateMachine = Class
  Protected
      _StateIndex:Integer;
      _StateBuffer:Array[1..MaxStates] Of Integer;
      _CurrentState:Integer;

      Procedure yybegin(newState:Integer);
      Procedure yypushstate(State:Integer);
      Procedure yypopstate();

      Function yylaststate:Integer;

      Procedure switchstate(State:Integer);
      Procedure switchCallback(Callback, GotoState:Integer);

      Procedure yyerror(Const S:AnsiString);

      Procedure yyaction(yyruleno : Integer ); Virtual; Abstract;

  Public
    Constructor Create();
    Destructor Destroy; Override;
  End;

Implementation
Uses TERRA_Error;

{ StateMachine }
Constructor StateMachine.Create();
Begin
  inherited Create;

  _StateIndex := 0;
End;

Procedure StateMachine.yybegin(newState:Integer);
Begin
  _CurrentState := newState;
End;

Procedure StateMachine.yyPopState();
Begin
  If (_StateIndex>0) Then
  Begin
    //Result := _StateBuffer[_StateIndex];
    yybegin(_StateBuffer[_StateIndex]);
    Dec(_StateIndex);
  End;
End;

Procedure StateMachine.yyPushstate(State: Integer);
Begin
  Inc(_StateIndex);
  _StateBuffer[_StateIndex] := State;
  yybegin(State);
End;

Procedure StateMachine.switchCallback(Callback, GotoState: Integer);
Begin
  yyPopState();
  yyPushState(Callback);
  yyPushState(GotoState);
  yyBegin(GotoState);
End;

Procedure StateMachine.switchstate(State: Integer);
Begin
  yyPopState();
	yyPushState(state);
	yyBegin(state);
End;

// If `yywrap()' returns false (zero), then it is assumed that the function has gone ahead and set up yyin to point to another input file, and scanning continues.
// If it returns true (non-zero), then the scanner terminates, returning 0 to its caller.
Procedure StateMachine.yyerror(const S: AnsiString);
Begin
  RaiseError(S);
End;

Function StateMachine.yylaststate: Integer;
Begin
  If (_StateIndex>1) Then
    Result := _StateBuffer[Pred(_StateIndex)]
  Else
    Result := 0;
End;

Destructor StateMachine.Destroy;
Begin
  // do nothing
End;

End.
