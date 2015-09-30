inherited OverviewFrame: TOverviewFrame
  OnResize = FrameResize
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 53
      Caption = 'Overview'
      ExplicitWidth = 53
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
  object pnlGroupings: TPanel
    Left = 0
    Top = 27
    Width = 318
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblGroupings: TLabel
      Left = 4
      Top = 6
      Width = 76
      Height = 13
      Caption = '&Group snippets:'
      FocusControl = cbGroupings
    end
    object cbGroupings: TComboBox
      Left = 88
      Top = 8
      Width = 169
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbGroupingsChange
    end
  end
  object pnlSnippets: TPanel
    Left = 0
    Top = 68
    Width = 318
    Height = 170
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
    ExplicitLeft = 64
    ExplicitTop = 96
    ExplicitWidth = 225
    ExplicitHeight = 113
    object tvSnippets: TTreeView
      AlignWithMargins = True
      Left = 0
      Top = 2
      Width = 316
      Height = 166
      Margins.Left = 0
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BorderStyle = bsNone
      HideSelection = False
      HotTrack = True
      Indent = 12
      PopupMenu = mnuOverview
      ReadOnly = True
      ShowLines = False
      TabOrder = 0
      OnChanging = tvSnippetsChanging
      OnCreateNodeClass = tvSnippetsCreateNodeClass
      OnDeletion = tvSnippetsDeletion
      OnEnter = tvSnippetsEnter
      OnKeyDown = tvSnippetsKeyDown
      OnKeyPress = tvSnippetsKeyPress
      OnKeyUp = tvSnippetsKeyUp
      OnMouseDown = tvSnippetsMouseDown
      ExplicitLeft = 11
      ExplicitTop = 25
      ExplicitWidth = 154
      ExplicitHeight = 88
    end
  end
  object mnuOverview: TPopupMenu
    AutoPopup = False
    Left = 72
    Top = 144
  end
end
