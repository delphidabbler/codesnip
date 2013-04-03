inherited UpdatePrefsFrame: TUpdatePrefsFrame
  HelpType = htKeyword
  HelpKeyword = 'UpdatePrefs'
  object lblProgAutoCheckFreq: TLabel
    Left = 3
    Top = 12
    Width = 135
    Height = 13
    Caption = 'Check for &program updates:'
    FocusControl = cbProgAutoCheckFreq
  end
  object lblDBAutoCheckFreq: TLabel
    Left = 3
    Top = 52
    Width = 140
    Height = 13
    Caption = 'Check for &database updates:'
    FocusControl = cbDBAutoCheckFreq
  end
  object cbProgAutoCheckFreq: TComboBox
    Left = 172
    Top = 9
    Width = 124
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CBAutoCheckFreqChanged
  end
  object cbDBAutoCheckFreq: TComboBox
    Left = 172
    Top = 49
    Width = 124
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CBAutoCheckFreqChanged
  end
end
