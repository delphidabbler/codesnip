inherited CompilersDlgNamespacesFrame: TCompilersDlgNamespacesFrame
  object lblExplainNamespaces: TLabel
    Left = 160
    Top = 69
    Width = 137
    Height = 116
    AutoSize = False
    Caption = 
      'Enter a new namespace in the edit box and click Add. Click a nam' +
      'espace to edit then click Replace. Delete removes selected names' +
      'pace.'
    WordWrap = True
  end
  object lblNamespaces: TLabel
    Left = 3
    Top = 52
    Width = 64
    Height = 13
    Caption = '&Namespaces:'
    FocusControl = lbNamespaces
  end
  object lblNamespace: TLabel
    Left = 4
    Top = 4
    Width = 114
    Height = 13
    Caption = 'Add or &edit namespace:'
    FocusControl = edNamespace
  end
  object btnDefSwitches: TButton
    Left = 232
    Top = 38
    Width = 65
    Height = 25
    Caption = 'Defa&ults'
    TabOrder = 5
    OnClick = btnDefSwitchesClick
  end
  object lbNamespaces: TListBox
    Left = 3
    Top = 69
    Width = 142
    Height = 121
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbNamespacesClick
  end
  object edNamespace: TEdit
    Left = 4
    Top = 21
    Width = 142
    Height = 21
    TabOrder = 0
    OnChange = edNamespaceChange
  end
  object btnAdd: TButton
    Left = 161
    Top = 7
    Width = 65
    Height = 25
    Caption = '&Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnReplace: TButton
    Left = 232
    Top = 7
    Width = 65
    Height = 25
    Caption = '&Replace'
    TabOrder = 3
    OnClick = btnReplaceClick
  end
  object btnDelete: TButton
    Left = 161
    Top = 38
    Width = 65
    Height = 25
    Caption = 'De&lete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
end
