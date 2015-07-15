object Form1: TForm1
  Left = 238
  Top = 138
  Width = 883
  Height = 744
  Caption = 'Parse Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object TreeView1: TTreeView
    Left = 432
    Top = 80
    Width = 353
    Height = 617
    Indent = 19
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 16
    Top = 32
    Width = 249
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 272
    Top = 32
    Width = 41
    Height = 25
    Caption = '...'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 336
    Top = 24
    Width = 97
    Height = 41
    Caption = 'Parse'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 80
    Width = 409
    Height = 617
    TabOrder = 4
  end
end
