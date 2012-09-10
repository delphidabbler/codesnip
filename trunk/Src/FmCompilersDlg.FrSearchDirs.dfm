inherited CompilersDlgSearchDirsFrame: TCompilersDlgSearchDirsFrame
  Width = 313
  ExplicitWidth = 313
  object lblPaths: TLabel
    Left = 3
    Top = 3
    Width = 71
    Height = 13
    Caption = 'Existing &Paths:'
    FocusControl = lbPaths
  end
  object lblPath: TLabel
    Left = 3
    Top = 103
    Width = 82
    Height = 13
    Caption = 'Add or edit pat&h:'
    FocusControl = edPath
  end
  object btnUp: TSpeedButton
    Left = 284
    Top = 22
    Width = 23
    Height = 22
    Action = actUp
  end
  object btnDown: TSpeedButton
    Left = 284
    Top = 67
    Width = 23
    Height = 22
    Action = actDown
  end
  object lbPaths: TListBox
    Left = 3
    Top = 22
    Width = 271
    Height = 67
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbPathsClick
  end
  object edPath: TEdit
    Left = 3
    Top = 122
    Width = 271
    Height = 21
    TabOrder = 1
  end
  object btnBrowse: TButton
    Left = 280
    Top = 122
    Width = 27
    Height = 21
    Action = actBrowse
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object btnAdd: TButton
    Left = 66
    Top = 149
    Width = 60
    Height = 25
    Action = actAdd
    TabOrder = 4
  end
  object btnDelete: TButton
    Left = 132
    Top = 149
    Width = 60
    Height = 25
    Action = actDelete
    TabOrder = 5
  end
  object btnReplace: TButton
    Left = 0
    Top = 149
    Width = 60
    Height = 25
    Action = actReplace
    TabOrder = 3
  end
  object ilActions: TImageList
    Left = 8
    Top = 184
  end
  object alFrame: TActionList
    Images = ilActions
    Left = 56
    Top = 184
    object actUp: TAction
      ImageIndex = 43
      ShortCut = 16422
      OnExecute = actUpExecute
      OnUpdate = actUpUpdate
    end
    object actDown: TAction
      ImageIndex = 44
      ShortCut = 16424
      OnExecute = actDownExecute
      OnUpdate = actDownUpdate
    end
    object actBrowse: TAction
      Caption = '...'
      OnExecute = actBrowseExecute
    end
    object actAdd: TAction
      Caption = '&Add'
      OnExecute = actAddExecute
      OnUpdate = actAddUpdate
    end
    object actReplace: TAction
      Caption = '&Replace'
      OnExecute = actReplaceExecute
      OnUpdate = actReplaceUpdate
    end
    object actDelete: TAction
      Caption = 'D&elete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
  end
end
