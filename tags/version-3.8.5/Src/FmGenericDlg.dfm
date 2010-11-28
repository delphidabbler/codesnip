inherited GenericDlg: TGenericDlg
  Left = 215
  Top = 123
  BorderStyle = bsDialog
  Caption = 'GenericDlg'
  ClientHeight = 328
  ClientWidth = 468
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object bvlBottom: TBevel
    Left = 8
    Top = 296
    Width = 377
    Height = 2
    Shape = bsTopLine
  end
  object pnlBody: TPanel
    Left = 8
    Top = 8
    Width = 377
    Height = 281
    BevelOuter = bvNone
    TabOrder = 0
  end
  object btnHelp: TButton
    Left = 312
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
