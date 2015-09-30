object TagsEditorFrame: TTagsEditorFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnResize = FrameResize
  object sbTagEditors: TScrollBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    HorzScrollBar.Visible = False
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ParentBackground = True
    TabOrder = 0
    object pnlEditors: TPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 41
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object mnuEditor: TPopupMenu
    Left = 23
    Top = 40
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
    object miMore: TMenuItem
      Action = actMore
    end
  end
  object alEditor: TActionList
    Left = 79
    Top = 40
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
    object actMore: TAction
      Caption = 'Add Row...'
      Hint = 'Add Row|Adds a new row of empty tag edit controls'
      ShortCut = 24641
      OnExecute = actMoreExecute
    end
  end
end
