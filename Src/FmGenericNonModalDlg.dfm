inherited GenericNonModalDlg: TGenericNonModalDlg
  Caption = 'GenericNonModalDlg'
  ClientWidth = 979
  ExplicitWidth = 991
  PixelsPerInch = 168
  TextHeight = 30
  inherited btnHelp: TButton
    TabOrder = 2
  end
  object btnClose: TButton
    Left = 686
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnCloseClick
  end
end
