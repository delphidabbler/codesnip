inherited CompilersDlgCompilerFrame: TCompilersDlgCompilerFrame
  Width = 313
  Height = 137
  ExplicitWidth = 313
  ExplicitHeight = 137
  object lblCompilerPath: TLabel
    Left = 4
    Top = 4
    Width = 174
    Height = 13
    Caption = 'Enter compiler &executable file name:'
    FocusControl = edCompilerPath
  end
  object edCompilerPath: TEdit
    Left = 5
    Top = 24
    Width = 271
    Height = 21
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnExit = edCompilerPathExit
  end
  object btnBrowse: TButton
    Left = 282
    Top = 24
    Width = 27
    Height = 21
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnClear: TButton
    Left = 3
    Top = 51
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 2
    OnClick = btnClearClick
  end
end
