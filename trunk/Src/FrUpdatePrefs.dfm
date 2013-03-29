inherited UpdatePrefsFrame: TUpdatePrefsFrame
  HelpType = htKeyword
  HelpKeyword = 'UpdatePrefs'
  object chkAutoCheckProgramUpdates: TCheckBox
    Left = 3
    Top = 3
    Width = 310
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check for &program updates on start-up'
    TabOrder = 0
  end
  object chkAutoCheckDatabaseUpdates: TCheckBox
    Left = 3
    Top = 26
    Width = 310
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check for &database updates on start-up'
    TabOrder = 1
  end
end
