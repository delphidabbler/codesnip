inherited CompilersDlgNamespacesFrame: TCompilersDlgNamespacesFrame
  object lblExplainNamespaces: TLabel
    Left = 221
    Top = 69
    Width = 137
    Height = 116
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 
      'Enter a new namespace in the edit box and click Add. Click a nam' +
      'espace to edit then click Replace. Delete removes selected names' +
      'pace.'
    WordWrap = True
    ExplicitLeft = 160
  end
  object lblNamespaces: TLabel
    Left = 4
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
    Left = 293
    Top = 38
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Defa&ults'
    TabOrder = 5
    OnClick = btnDefSwitchesClick
    ExplicitLeft = 232
  end
  object lbNamespaces: TListBox
    Left = 4
    Top = 69
    Width = 196
    Height = 160
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbNamespacesClick
  end
  object edNamespace: TEdit
    Left = 4
    Top = 21
    Width = 196
    Height = 21
    TabOrder = 0
    OnChange = edNamespaceChange
  end
  object btnAdd: TButton
    Left = 222
    Top = 7
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 2
    OnClick = btnAddClick
    ExplicitLeft = 161
  end
  object btnReplace: TButton
    Left = 293
    Top = 7
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Replace'
    TabOrder = 3
    OnClick = btnReplaceClick
    ExplicitLeft = 232
  end
  object btnDelete: TButton
    Left = 222
    Top = 38
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'De&lete'
    TabOrder = 4
    OnClick = btnDeleteClick
    ExplicitLeft = 161
  end
end
