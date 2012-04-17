inherited GenericOKDlg: TGenericOKDlg
  Caption = 'GenericOKDlg'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 369
    Height = 305
    ExplicitWidth = 369
    ExplicitHeight = 305
  end
  inherited btnHelp: TButton
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 232
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 153
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
