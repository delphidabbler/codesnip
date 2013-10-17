inherited HiliterPrefsFrame: THiliterPrefsFrame
  Width = 393
  Height = 326
  ExplicitWidth = 393
  ExplicitHeight = 326
  DesignSize = (
    393
    326)
  object lblUITheme: TLabel
    Left = 3
    Top = 51
    Width = 72
    Height = 13
    Caption = 'User interface:'
  end
  object lblExportTheme: TLabel
    Left = 3
    Top = 85
    Width = 74
    Height = 13
    Caption = 'Exported code:'
  end
  object lblPrintTheme: TLabel
    Left = 3
    Top = 123
    Width = 40
    Height = 13
    Caption = 'Printing:'
  end
  object lblInstructions: TLabel
    Left = 3
    Top = 0
    Width = 281
    Height = 13
    Caption = 'Choose syntax highlighter themes to use for the following:'
  end
  object btnReset: TButton
    Left = 95
    Top = 160
    Width = 146
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Restore Defaults'
    TabOrder = 0
    OnClick = btnResetClick
  end
  object cbUITheme: TComboBox
    Left = 96
    Top = 51
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = ThemeComboChange
  end
  object cbExportTheme: TComboBox
    Left = 96
    Top = 82
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ThemeComboChange
  end
  object cbPrintTheme: TComboBox
    Left = 96
    Top = 120
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ThemeComboChange
  end
  object btnEditThemes: TButton
    Left = 95
    Top = 199
    Width = 145
    Height = 25
    Caption = 'Edit Themes...'
    TabOrder = 4
    OnClick = btnEditThemesClick
  end
end
