inherited DisplayPrefsFrame: TDisplayPrefsFrame
  Width = 438
  Height = 347
  ExplicitWidth = 438
  ExplicitHeight = 347
  object lblOverviewTree: TLabel
    Left = 16
    Top = 6
    Width = 161
    Height = 13
    Caption = 'Start &overview pane treeview as:'
    FocusControl = cbOverviewTree
  end
  object lblGroupHeadingColour: TLabel
    Left = 16
    Top = 112
    Width = 191
    Height = 13
    Caption = '&Group heading colour in overview pane:'
  end
  object lblCollectionColours: TLabel
    Left = 16
    Top = 139
    Width = 240
    Height = 13
    Caption = 'Heading colour snippets from different &collections:'
    FocusControl = cbCollection
  end
  object lblSourceBGColour: TLabel
    Left = 16
    Top = 200
    Width = 170
    Height = 13
    Caption = 'Background colour for &source code:'
  end
  object lblOverviewFontSize: TLabel
    Left = 16
    Top = 253
    Width = 145
    Height = 13
    Caption = 'Overview tree view &font size: '
    FocusControl = cbOverviewFontSize
  end
  object lblDetailFontSize: TLabel
    Left = 16
    Top = 280
    Width = 105
    Height = 13
    Caption = 'Detail pane font si&ze: '
    FocusControl = cbDetailFontSize
  end
  object lblHiliterInfo: TLabel
    Left = 16
    Top = 304
    Width = 358
    Height = 26
    Caption = 
      'To change the size of the source code font use the the Syntax Hi' +
      'ghlighter options page.'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    WordWrap = True
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
    Top = 219
    Width = 192
    Height = 25
    Caption = 'Use &Default Colours'
    TabOrder = 3
    OnClick = btnDefColoursClick
  end
  object cbOverviewFontSize: TComboBox
    Left = 192
    Top = 250
    Width = 57
    Height = 21
    TabOrder = 4
    OnChange = FontSizeChange
  end
  object cbDetailFontSize: TComboBox
    Left = 192
    Top = 277
    Width = 57
    Height = 21
    TabOrder = 5
    OnChange = FontSizeChange
  end
  object cbCollection: TComboBox
    Left = 16
    Top = 158
    Width = 170
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    OnChange = cbCollectionChange
  end
end
