inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ClientHeight = 362
  ClientWidth = 471
  ExplicitWidth = 477
  ExplicitHeight = 388
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 449
    Height = 249
    ExplicitWidth = 449
    ExplicitHeight = 249
    object pbCompiler: TPaintBox
      Left = 128
      Top = 0
      Width = 321
      Height = 23
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      OnPaint = pbCompilerPaint
    end
    object lbCompilers: TListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 244
      Style = lbOwnerDrawFixed
      Ctl3D = True
      IntegralHeight = True
      ItemHeight = 48
      ParentCtl3D = False
      TabOrder = 0
      OnClick = lbCompilersClick
      OnDrawItem = lbCompilersDrawItem
    end
    object pcCompiler: TPageControl
      Left = 127
      Top = 32
      Width = 321
      Height = 212
      ActivePage = tsExecFile
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsExecFile: TTabSheet
        Caption = '&Compiler'
        object lblCompilerPath: TLabel
          Left = 4
          Top = 4
          Width = 170
          Height = 13
          Caption = 'Enter compiler e&xecutable file name:'
        end
        object edCompilerPath: TEdit
          Left = 3
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
          Caption = 'Cle&ar'
          TabOrder = 2
          OnClick = btnClearClick
        end
      end
      object tsSwitches: TTabSheet
        Caption = 'Co&mmand Line'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
          Caption = 'S&witches:'
          FocusControl = lbSwitches
        end
        object lblSwitch: TLabel
          Left = 4
          Top = 4
          Width = 87
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
          Caption = 'A&dd'
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
      object tsOutputLog: TTabSheet
        Caption = 'Output &Log'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 136
        object lblLogPrefixes: TLabel
          Left = 4
          Top = 4
          Width = 77
          Height = 13
          Caption = 'Edit log &prefixes:'
          FocusControl = vleLogPrefixes
        end
        object vleLogPrefixes: TValueListEditor
          Left = 4
          Top = 24
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
          RowHeights = (
            17
            19
            18)
        end
      end
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  inherited btnCancel: TButton
    TabOrder = 3
  end
  inherited btnOK: TButton
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnDetect: TButton
    Left = 8
    Top = 304
    Width = 145
    Height = 25
    Caption = '&Auto Detect Compilers'
    TabOrder = 1
    OnClick = btnDetectClick
  end
end
