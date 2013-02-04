inherited GenericNonModalDlg: TGenericNonModalDlg
  BorderStyle = bsToolWindow
  Caption = 'GenericNonModalDlg'
  ClientHeight = 350
  ClientWidth = 468
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 232
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnCloseClick
  end
end
