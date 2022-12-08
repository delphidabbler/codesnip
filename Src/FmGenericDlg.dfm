inherited GenericDlg: TGenericDlg
  Left = 215
  Top = 123
  Caption = 'GenericDlg'
  ClientHeight = 762
  ClientWidth = 991
  ParentFont = True
  ExplicitWidth = 1003
  ExplicitHeight = 824
  PixelsPerInch = 168
  TextHeight = 30
  object bvlBottom: TBevel
    Left = 24
    Top = 683
    Width = 957
    Height = 15
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Shape = bsTopLine
  end
  object pnlBody: TPanel
    Left = 14
    Top = 14
    Width = 957
    Height = 631
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BevelOuter = bvNone
    TabOrder = 0
  end
  object btnHelp: TButton
    Left = 840
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Help'
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
