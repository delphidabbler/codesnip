inherited GenericOKDlg: TGenericOKDlg
  Caption = 'GenericOKDlg'
  ClientWidth = 979
  ExplicitWidth = 991
  PixelsPerInch = 168
  TextHeight = 30
  inherited btnHelp: TButton
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 699
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 559
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
