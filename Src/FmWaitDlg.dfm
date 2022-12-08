inherited WaitDlg: TWaitDlg
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'WaitDlg'
  ClientHeight = 151
  ClientWidth = 294
  OnClose = FormClose
  ExplicitWidth = 294
  ExplicitHeight = 151
  PixelsPerInch = 168
  TextHeight = 30
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 151
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 0
    object lblCaption: TLabel
      Left = 7
      Top = 44
      Width = 102
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'lblCaption'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
end
