inherited CompilersDlgCompilerFrame: TCompilersDlgCompilerFrame
  DesignSize = (
    362
    235)
  object lblCompilerPath: TLabel
    Left = 4
    Top = 4
    Width = 174
    Height = 13
    Caption = 'Enter compiler &executable file name:'
    FocusControl = edCompilerPath
  end
  object edCompilerPath: TEdit
    Left = 4
    Top = 24
    Width = 320
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnExit = edCompilerPathExit
  end
  object btnBrowse: TButton
    Left = 331
    Top = 24
    Width = 27
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnClear: TButton
    Left = 4
    Top = 51
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object chkShowInMain: TCheckBox
    Left = 4
    Top = 88
    Width = 320
    Height = 17
    Caption = 'Display &results for this compiler in details pane'
    TabOrder = 3
  end
  object chkPermitAutoDetect: TCheckBox
    Left = 4
    Top = 111
    Width = 320
    Height = 17
    Caption = 'Permit &auto-detection && registration of this compiler'
    TabOrder = 4
  end
end
