inherited PreferencesDlg: TPreferencesDlg
  Caption = 'Preferences'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object pcMain: TPageControl
      Left = 176
      Top = 0
      Width = 781
      Height = 631
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      MultiLine = True
      TabOrder = 1
    end
    object lbPages: TListBox
      Left = 0
      Top = 0
      Width = 268
      Height = 631
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      ItemHeight = 30
      TabOrder = 0
      OnClick = lbPagesClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
