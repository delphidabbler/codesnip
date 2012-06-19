inherited OverviewFrame: TOverviewFrame
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 54
      Caption = 'Overview'
      ExplicitWidth = 54
    end
    object tbarOverview: TToolBar
      Left = 224
      Top = 0
      Width = 94
      Height = 23
      Align = alRight
      AutoSize = True
      Caption = 'tbarOverview'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object tcDisplayStyle: TTabControl
    Left = 0
    Top = 27
    Width = 318
    Height = 211
    Align = alClient
    MultiLine = True
    TabOrder = 1
    Tabs.Strings = (
      'Categorised'
      'Alphabetical'
      'Snippet Kind')
    TabIndex = 0
    OnChange = tcDisplayStyleChange
    OnChanging = tcDisplayStyleChanging
    OnMouseDown = tcDisplayStyleMouseDown
    ExplicitTop = 29
    object tvSnippets: TTreeView
      Left = 4
      Top = 24
      Width = 310
      Height = 183
      Align = alClient
      BorderStyle = bsNone
      HideSelection = False
      Indent = 19
      PopupMenu = mnuOverview
      ReadOnly = True
      ShowButtons = False
      ShowRoot = False
      TabOrder = 0
      OnChanging = tvSnippetsChanging
      OnCreateNodeClass = tvSnippetsCreateNodeClass
      OnDeletion = tvSnippetsDeletion
      OnEnter = tvSnippetsEnter
      OnKeyDown = tvSnippetsKeyDown
      OnKeyPress = tvSnippetsKeyPress
      OnKeyUp = tvSnippetsKeyUp
      OnMouseDown = tvSnippetsMouseDown
    end
  end
  object mnuOverview: TPopupMenu
    Left = 64
    Top = 72
  end
end
