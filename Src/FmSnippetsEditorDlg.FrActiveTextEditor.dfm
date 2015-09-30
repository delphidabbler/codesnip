object SnippetsActiveTextEdFrame: TSnippetsActiveTextEdFrame
  Left = 0
  Top = 0
  Width = 454
  Height = 116
  Color = clWindow
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  OnEnter = FrameEnter
  object edText: TMemo
    Left = 0
    Top = 0
    Width = 454
    Height = 93
    Align = alClient
    PopupMenu = mnuEditor
    ScrollBars = ssVertical
    TabOrder = 0
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
  end
  object alEditor: TActionList
    Left = 47
    Top = 32
    object actConvertToPlainText: TAction
      Caption = 'Convert To Plain Text'
      Hint = 
        'Convert To Plain Text|Converts REML text into plain text, loosin' +
        'g any formatting'
      OnExecute = actConvertToPlainTextExecute
      OnUpdate = actConvertToPlainTextUpdate
    end
    object actConvertToREML: TAction
      Caption = 'Convert To Markup'
      Hint = 
        'Convert To Markup|Converts the plain text into the equivalent RE' +
        'ML markup'
      OnExecute = actConvertToREMLExecute
      OnUpdate = actConvertToREMLUpdate
    end
    object actCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ShortCut = 16472
    end
    object actCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ShortCut = 16451
    end
    object actPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ShortCut = 16470
    end
    object actSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects all the text'
      ShortCut = 16449
    end
    object actUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ShortCut = 16474
    end
  end
  object mnuEditor: TPopupMenu
    Left = 7
    Top = 32
    object miCut: TMenuItem
      Action = actCut
    end
    object miCopy: TMenuItem
      Action = actCopy
    end
    object miPaste: TMenuItem
      Action = actPaste
    end
    object miSpace1: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Action = actSelectAll
    end
    object miSpacer2: TMenuItem
      Caption = '-'
    end
    object miUndo: TMenuItem
      Action = actUndo
    end
    object miSpacer3: TMenuItem
      Caption = '-'
    end
    object miConvertToPlainText: TMenuItem
      Action = actConvertToPlainText
    end
    object miConvertToREML: TMenuItem
      Action = actConvertToREML
    end
  end
end
