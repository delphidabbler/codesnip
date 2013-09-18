inherited CodeGenPrefsFrame: TCodeGenPrefsFrame
  Width = 426
  Height = 311
  ExplicitWidth = 426
  ExplicitHeight = 311
  object lblSymbol: TLabel
    Left = 0
    Top = 196
    Width = 38
    Height = 13
    Caption = '&Symbol:'
    FocusControl = edSymbol
  end
  object lblMinCompiler: TLabel
    Left = 0
    Top = 223
    Width = 68
    Height = 13
    Caption = 'Min. &Compiler:'
    FocusControl = edMinCompiler
  end
  object chkSwitchOff: TCheckBox
    Left = 0
    Top = 0
    Width = 414
    Height = 17
    Caption = 'Emit $&WARN directives to switch off listed warnings'
    TabOrder = 0
    OnClick = chkSwitchOffClick
  end
  object lvWarnings: TListView
    Left = 0
    Top = 26
    Width = 414
    Height = 150
    Columns = <
      item
        Caption = 'Symbol'
        Width = 300
      end
      item
        Caption = 'Min. Compiler'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnClick = lvWarningsClick
  end
  object edSymbol: TEdit
    Left = 88
    Top = 193
    Width = 273
    Height = 21
    TabOrder = 3
  end
  object edMinCompiler: TEdit
    Left = 88
    Top = 220
    Width = 121
    Height = 21
    TabOrder = 4
    OnKeyPress = edMinCompilerKeyPress
  end
  object btnAdd: TButton
    Left = 88
    Top = 247
    Width = 65
    Height = 25
    Action = actAdd
    TabOrder = 6
  end
  object btnDelete: TButton
    Left = 230
    Top = 247
    Width = 65
    Height = 25
    Action = actDelete
    TabOrder = 8
  end
  object btnUpdate: TButton
    Left = 159
    Top = 247
    Width = 65
    Height = 25
    Action = actUpdate
    TabOrder = 7
  end
  object btnPreview: TButton
    Left = 339
    Top = 3
    Width = 75
    Height = 25
    Action = actPreview
    TabOrder = 1
  end
  object btnPredefined: TBitBtn
    Left = 215
    Top = 216
    Width = 106
    Height = 25
    Caption = 'Pre-defined'
    DoubleBuffered = True
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFF
      FFFFFFFFFF0000FFFFFFFFFFF000000FFFFFFFFF00000000FFFFFFF000000000
      0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    Layout = blGlyphRight
    ParentDoubleBuffered = False
    TabOrder = 5
    OnClick = btnPredefinedClick
  end
  object actMain: TActionList
    Left = 328
    Top = 120
    object actAdd: TAction
      Caption = '&Add'
      OnExecute = actAddExecute
      OnUpdate = actAddUpdate
    end
    object actUpdate: TAction
      Caption = '&Update'
      OnExecute = actUpdateExecute
      OnUpdate = actUpdateUpdate
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actPreview: TAction
      Caption = '&Preview...'
      OnExecute = actPreviewExecute
      OnUpdate = actPreviewUpdate
    end
  end
  object mnuPreDefCompilers: TPopupMenu
    Left = 192
    Top = 136
  end
end
