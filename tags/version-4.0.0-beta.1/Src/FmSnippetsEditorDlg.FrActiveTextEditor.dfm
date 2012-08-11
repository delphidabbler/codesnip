object SnippetsActiveTextEdFrame: TSnippetsActiveTextEdFrame
  Left = 0
  Top = 0
  Width = 454
  Height = 116
  Color = clWindow
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object edText: TMemo
    Left = 0
    Top = 0
    Width = 454
    Height = 93
    Align = alClient
    PopupMenu = mnuEdit
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = 46
    ExplicitHeight = 95
  end
  object tcEditMode: TTabControl
    Left = 0
    Top = 93
    Width = 454
    Height = 23
    Align = alBottom
    Style = tsFlatButtons
    TabOrder = 1
    Tabs.Strings = (
      'Plain Text'
      'Markup')
    TabIndex = 0
    OnChange = tcEditModeChange
    ExplicitTop = 0
  end
  object alEditor: TActionList
    Left = 47
    Top = 29
    object actConvertToPlainText: TAction
      Caption = 'Convert To Plain Text'
      OnExecute = actConvertToPlainTextExecute
      OnUpdate = actConvertToPlainTextUpdate
    end
    object actConvertToREML: TAction
      Caption = 'Convert To Markup'
      OnExecute = actConvertToREMLExecute
      OnUpdate = actConvertToREMLUpdate
    end
    object actSwitchToPlainTextMode: TAction
      Caption = 'Plain Text'
      OnExecute = actSwitchToPlainTextModeExecute
    end
    object actSwitchToREMLMode: TAction
      Caption = 'Markup'
      OnExecute = actSwitchToREMLModeExecute
    end
  end
  object mnuEdit: TPopupMenu
    Left = 7
    Top = 29
    object miConvertToPlainText: TMenuItem
      Action = actConvertToPlainText
    end
    object miConvertToREML: TMenuItem
      Action = actConvertToREML
    end
  end
end
