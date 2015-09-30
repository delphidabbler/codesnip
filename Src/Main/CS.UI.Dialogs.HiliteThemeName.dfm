inherited HiliteThemeNameDlg: THiliteThemeNameDlg
  Caption = 'Name Syntax Highlighter Theme'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblThemeName: TLabel
      Left = 0
      Top = 0
      Width = 65
      Height = 13
      Caption = '&Theme name:'
      FocusControl = cbThemeName
    end
    object lblError: TLabel
      Left = 0
      Top = 72
      Width = 229
      Height = 13
      Caption = 'This theme is reserved and can'#39't be overwritten'
    end
    object cbThemeName: TComboBox
      Left = 0
      Top = 32
      Width = 329
      Height = 21
      TabOrder = 0
      Text = 'cbThemeName'
      OnChange = cbThemeNameChange
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
