inherited NewHiliterNameDlg: TNewHiliterNameDlg
  Caption = 'Save Highlighter'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblNames: TLabel
      Left = 0
      Top = 0
      Width = 133
      Height = 13
      Caption = '&Save current highlighter as:'
      FocusControl = cbNames
    end
    object lblHelp: TLabel
      Left = 0
      Top = 46
      Width = 121
      Height = 13
      Caption = '(Maximum 32 characters)'
    end
    object cbNames: TComboBox
      Left = 0
      Top = 19
      Width = 193
      Height = 21
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
