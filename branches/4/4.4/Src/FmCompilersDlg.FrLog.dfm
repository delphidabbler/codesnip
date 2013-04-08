inherited CompilersDlgLogFrame: TCompilersDlgLogFrame
  object lblLogPrefixes: TLabel
    Left = 4
    Top = 4
    Width = 81
    Height = 13
    Caption = 'Edit log &prefixes:'
    FocusControl = vleLogPrefixes
  end
  object vleLogPrefixes: TValueListEditor
    Left = 4
    Top = 22
    Width = 354
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    DefaultColWidth = 20
    DefaultDrawing = False
    DisplayOptions = [doKeyColFixed]
    FixedColor = clMenuBar
    FixedCols = 1
    Options = [goEditing, goAlwaysShowEditor]
    ParentCtl3D = False
    ScrollBars = ssNone
    Strings.Strings = (
      'Fatal Error='
      'Error='
      'Warning=')
    TabOrder = 0
    TitleCaptions.Strings = (
      'Message Type'
      'Prefix')
    OnDrawCell = vleLogPrefixesDrawCell
    OnSelectCell = vleLogPrefixesSelectCell
    ExplicitWidth = 304
    ColWidths = (
      20
      278)
    RowHeights = (
      17
      19
      18)
  end
end
