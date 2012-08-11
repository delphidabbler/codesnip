inherited DisplayPrefsFrame: TDisplayPrefsFrame
  Width = 397
  Height = 311
  ExplicitWidth = 397
  ExplicitHeight = 311
  object lblOverviewTree: TLabel
    Left = 16
    Top = 3
    Width = 161
    Height = 13
    Caption = 'Start overview pane treeview as:'
  end
  object lblMainColour: TLabel
    Left = 16
    Top = 96
    Width = 193
    Height = 13
    Caption = 'Heading colour for main database items:'
  end
  object lblUserColour: TLabel
    Left = 16
    Top = 115
    Width = 192
    Height = 13
    Caption = 'Heading colour for user database items:'
  end
  object cbOverviewTree: TComboBox
    Left = 192
    Top = -1
    Width = 113
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object chkHideEmptySections: TCheckBox
    Left = 16
    Top = 57
    Width = 353
    Height = 17
    Caption = 'Hide empty section headings in overview'
    TabOrder = 2
    OnClick = chkHideEmptySectionsClick
  end
  object chkSnippetsInNewTab: TCheckBox
    Left = 16
    Top = 34
    Width = 374
    Height = 17
    Caption = 'Display newly added snippets && categories in new tabs'
    TabOrder = 1
  end
end
