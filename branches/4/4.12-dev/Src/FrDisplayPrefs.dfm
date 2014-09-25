inherited DisplayPrefsFrame: TDisplayPrefsFrame
  Width = 397
  Height = 311
  ExplicitWidth = 397
  ExplicitHeight = 311
  object lblOverviewTree: TLabel
    Left = 16
    Top = 6
    Width = 161
    Height = 13
    Caption = 'Start &overview pane treeview as:'
    FocusControl = cbOverviewTree
  end
  object lblMainColour: TLabel
    Left = 16
    Top = 96
    Width = 193
    Height = 13
    Caption = 'Heading colour for &main database items:'
  end
  object lblUserColour: TLabel
    Left = 16
    Top = 115
    Width = 192
    Height = 13
    Caption = 'Heading colour for &user database items:'
  end
  object lblSourceBGColour: TLabel
    Left = 16
    Top = 134
    Width = 170
    Height = 13
    Caption = 'Background colour for &source code:'
  end
  object cbOverviewTree: TComboBox
    Left = 192
    Top = 2
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
    Caption = '&Hide empty section headings in overview'
    TabOrder = 2
    OnClick = chkHideEmptySectionsClick
  end
  object chkSnippetsInNewTab: TCheckBox
    Left = 16
    Top = 34
    Width = 374
    Height = 17
    Caption = 'Display newly added snippets && categories in new &tabs'
    TabOrder = 1
  end
  object btnDefColours: TButton
    Left = 16
    Top = 156
    Width = 192
    Height = 25
    Caption = 'Use Default &Colours'
    TabOrder = 3
    OnClick = btnDefColoursClick
  end
end
