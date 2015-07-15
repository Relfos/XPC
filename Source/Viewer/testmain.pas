unit testmain;

//https://pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  TERRA_String, TERRA_Utils, TERRA_Stream, XPC_Parser, TERRA_FileStream,
  TERRA_Log, llvmAPI;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    Procedure BuildNodes(Node:ASTNode; Target:TTreeNode);
  end;

var
  Form1: TForm1;

implementation

uses XPC_Delphi;

{$R *.dfm}

Var
  Parser:DelphiParser;
  AST:ASTNode;

Procedure MyLogHandler(Module, Desc:TERRAString); 
Begin
  Form1.Memo1.Lines.Add(Desc);
End;

Procedure TForm1.BuildNodes(Node:ASTNode; Target:TTreeNode);
Var
  I:Integer;
  Dest:TTreeNode;
  S, S2:TERRAString;
Begin
  If Node = Nil Then
    Exit;

  S := Node.ClassName;

  S2 := Node.GetValue();
  If S2<>'' Then
    S := S + ' ('+S2+')';

  Dest := TreeView1.Items.AddChild(Target, S) ;
  For I:=0 To Pred(Node.ChildrenCount) Do
    BuildNodes(Node.GetChildByIndex(I), Dest);
End;

procedure TForm1.Button2Click(Sender: TObject);
Var
  Source:Stream;
  S:TERRAString;
begin
  S := Edit1.Text;
  Source := FileStream.Open(S);

  Memo1.Clear();

  Try

  Parser := DelphiParser.Create;
  AST := Parser.Parse(Source, True);

  Except
  On E:ParsingException Do
    Begin
      Log(logConsole, 'Parser', E.Message);
      AST := Nil;
    End;
  End;

    ReleaseObject(Source);

    If Assigned(AST) Then
    Begin
      BuildNodes(AST, Nil);
      ReleaseObject(AST);
    End;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddLogFilter(logConsole, '', MyLogHandler);

  Edit1.Text := '../../Tests/Sources/test41.pas';
end;

end.
