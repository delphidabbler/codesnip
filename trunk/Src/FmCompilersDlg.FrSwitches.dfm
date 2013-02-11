inherited CompilersDlgSwitchesFrame: TCompilersDlgSwitchesFrame
  object lblExplainSwitches: TLabel
    Left = 221
    Top = 69
    Width = 137
    Height = 100
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 
      'Enter new switch in edit box and click Add. Click a switch to ed' +
      'it then click Replace. Delete removes selected switch.'
    WordWrap = True
  end
  object lblSwitches: TLabel
    Left = 4
    Top = 52
    Width = 46
    Height = 13
    Caption = '&Switches:'
    FocusControl = lbSwitches
  end
  object lblSwitch: TLabel
    Left = 4
    Top = 4
    Width = 90
    Height = 13
    Caption = 'Add or &edit switch:'
    FocusControl = edSwitch
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
    ExplicitLeft = 243
  end
  object lbSwitches: TListBox
    Left = 4
    Top = 69
    Width = 196
    Height = 160
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbSwitchesClick
  end
  object edSwitch: TEdit
    Left = 4
    Top = 21
    Width = 196
    Height = 21
    TabOrder = 0
    OnChange = edSwitchChange
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
    ExplicitLeft = 172
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
    ExplicitLeft = 243
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
    ExplicitLeft = 172
  end
end
