object SnippetsActiveTextEdFrame: TSnippetsActiveTextEdFrame
  Left = 0
  Top = 0
  Width = 454
  Height = 116
  TabOrder = 0
  DesignSize = (
    454
    116)
  object edText: TMemo
    Left = 0
    Top = 0
    Width = 454
    Height = 95
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = mnuEdit
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = -4
    ExplicitHeight = 97
  end
  object rbPlainTextMode: TRadioButton
    Left = 0
    Top = 97
    Width = 113
    Height = 17
    Action = actSwitchToPlainTextMode
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    TabStop = True
    ExplicitTop = 93
  end
  object rbREMLMode: TRadioButton
    Left = 111
    Top = 97
    Width = 113
    Height = 17
    Action = actSwitchToREMLMode
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    TabStop = True
    ExplicitTop = 93
  end
  object alEditor: TActionList
    Left = 63
    Top = 5
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
    Left = 15
    Top = 5
    object miConvertToPlainText: TMenuItem
      Action = actConvertToPlainText
    end
    object miConvertToREML: TMenuItem
      Action = actConvertToREML
    end
  end
end
