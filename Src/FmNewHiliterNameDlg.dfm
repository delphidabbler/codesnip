inherited NewHiliterNameDlg: TNewHiliterNameDlg
  Caption = 'Save Highlighter'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblNames: TLabel
      Left = 0
      Top = 0
      Width = 252
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Save current highlighter as:'
      FocusControl = cbNames
    end
    object lblHelp: TLabel
      Left = 0
      Top = 81
      Width = 234
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '(Maximum 32 characters)'
    end
    object cbNames: TComboBox
      Left = 0
      Top = 33
      Width = 338
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      MaxLength = 32
      TabOrder = 0
      Text = 'cbNames'
      OnChange = cbNamesChange
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
