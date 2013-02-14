inherited CompilersDlgLogFrame: TCompilersDlgLogFrame
  Width = 313
  ExplicitWidth = 313
  object lblLogPrefixes: TLabel
    Left = 4
    Top = 4
    Width = 81
    Height = 13
    Caption = 'Edit log &prefixes:'
    FocusControl = vleLogPrefixes
  end
  object vleLogPrefixes: TValueListEditor
    Left = 3
    Top = 23
    Width = 304
    Height = 59
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
    ColWidths = (
      20
      278)
    RowHeights = (
      17
      19
      18)
  end
end
