inherited NewsPrefsFrame: TNewsPrefsFrame
  object lblAgePrefix: TLabel
    Left = 0
    Top = 16
    Width = 116
    Height = 13
    Caption = '&Display news going back'
  end
  object lblAgeSuffix: TLabel
    Left = 184
    Top = 16
    Width = 121
    Height = 13
    Caption = 'days. (Min 14, Max 365).'
  end
  object seAge: TSpinEdit
    Left = 128
    Top = 13
    Width = 50
    Height = 22
    MaxValue = 365
    MinValue = 14
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    Value = 0
  end
end
