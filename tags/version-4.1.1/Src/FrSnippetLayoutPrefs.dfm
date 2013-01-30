inherited SnippetLayoutPrefsFrame: TSnippetLayoutPrefsFrame
  Width = 426
  Height = 311
  ParentShowHint = False
  ShowHint = True
  ExplicitWidth = 426
  ExplicitHeight = 311
  object btnIncludeFragment: TSpeedButton
    Left = 171
    Top = 107
    Width = 23
    Height = 22
    Action = actIncludeFragment
  end
  object btnExcludeFragment: TSpeedButton
    Left = 171
    Top = 135
    Width = 23
    Height = 22
    Action = actExcludeFragment
  end
  object btnMoveFragmentUp: TSpeedButton
    Left = 386
    Top = 107
    Width = 23
    Height = 22
    Action = actMoveFragmentUp
  end
  object btnMoveFragmentDown: TSpeedButton
    Left = 386
    Top = 135
    Width = 23
    Height = 22
    Action = actMoveFragmentDown
  end
  object lblInstructions: TLabel
    Left = 3
    Top = 3
    Width = 420
    Height = 39
    AutoSize = False
    Caption = 
      'Use this page to customise the appearance of snippets in the det' +
      'ails pane. Select a snippet kind then choose and order the items' +
      ' that are displayed.'
    WordWrap = True
  end
  object lblSnippetKinds: TLabel
    Left = 3
    Top = 48
    Width = 159
    Height = 13
    Caption = 'Edit page layout for snippet &kind:'
    FocusControl = cbSnippetKinds
  end
  object lblAvailable: TLabel
    Left = 3
    Top = 72
    Width = 128
    Height = 13
    Caption = '&Unused page components:'
    FocusControl = lbAvailableFragments
  end
  object lblUsed: TLabel
    Left = 216
    Top = 72
    Width = 138
    Height = 13
    Caption = '&Displayed page components:'
    FocusControl = lbUsedFragments
  end
  object cbSnippetKinds: TComboBox
    Left = 168
    Top = 45
    Width = 177
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbSnippetKindsChange
  end
  object lbAvailableFragments: TListBox
    Left = 3
    Top = 91
    Width = 162
    Height = 95
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
  end
  object lbUsedFragments: TListBox
    Left = 216
    Top = 91
    Width = 165
    Height = 95
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 2
  end
  object btnRestoreDefaults: TButton
    Left = 131
    Top = 194
    Width = 105
    Height = 25
    Action = actRestoreDefaults
    TabOrder = 3
  end
  object alFrame: TActionList
    Images = ilFrame
    Left = 272
    Top = 232
    object actMoveFragmentUp: TAction
      Hint = 'Move up'
      ImageIndex = 43
      ShortCut = 16422
      OnExecute = actMoveFragmentUpExecute
      OnUpdate = actMoveFragmentUpUpdate
    end
    object actMoveFragmentDown: TAction
      Hint = 'Move down'
      ImageIndex = 44
      ShortCut = 16424
      OnExecute = actMoveFragmentDownExecute
      OnUpdate = actMoveFragmentDownUpdate
    end
    object actIncludeFragment: TAction
      Hint = 'Include fragment'
      ImageIndex = 42
      ShortCut = 16423
      OnExecute = actIncludeFragmentExecute
      OnUpdate = actIncludeFragmentUpdate
    end
    object actExcludeFragment: TAction
      Hint = 'Exclude fragment'
      ImageIndex = 41
      ShortCut = 16421
      OnExecute = actExcludeFragmentExecute
      OnUpdate = actExcludeFragmentUpdate
    end
    object actRestoreDefaults: TAction
      Caption = '&Restore Defaults'
      OnExecute = actRestoreDefaultsExecute
    end
  end
  object ilFrame: TImageList
    Left = 312
    Top = 232
  end
end
