object CodeEditorFrame: TCodeEditorFrame
  Left = 0
  Top = 0
  Width = 773
  Height = 486
  TabOrder = 0
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
    object miRedo: TMenuItem
      Action = actRedo
    end
  end
  object alEditor: TActionList
    Left = 47
    Top = 32
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
    object actRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo|Re-performs the action undone by a previous Undo'
      ShortCut = 24666
      OnExecute = actRedoExecute
      OnUpdate = actRedoUpdate
    end
  end
end
