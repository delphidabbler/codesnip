inherited CompilersDlgSwitchesFrame: TCompilersDlgSwitchesFrame
  Width = 313
  Height = 209
  ExplicitWidth = 313
  ExplicitHeight = 209
  object lblExplainSwitches: TLabel
    Left = 160
    Top = 69
    Width = 137
    Height = 34
    AutoSize = False
    Caption = 
      'Enter new switch in edit box and click Add. Click a switch to ed' +
      'it then click Replace. Delete removes selected switch.'
    WordWrap = True
  end
  object lblSwitches: TLabel
    Left = 4
    Top = 45
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
  object lbSwitches: TListBox
    Left = 4
    Top = 62
    Width = 142
    Height = 95
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbSwitchesClick
  end
  object edSwitch: TEdit
    Left = 4
    Top = 21
    Width = 142
    Height = 21
    TabOrder = 0
    OnChange = edSwitchChange
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
    Caption = 'D&elete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
end
