inherited HiliteThemesEditorDlg: THiliteThemesEditorDlg
  Caption = 'Syntax Highlight Themes Editior'
  ClientHeight = 487
  ClientWidth = 609
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 615
  ExplicitHeight = 516
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvlBottom: TBevel
    Top = 446
    ExplicitTop = 446
  end
  inherited pnlBody: TPanel
    Width = 600
    Height = 439
    ExplicitWidth = 600
    ExplicitHeight = 439
    object lblThemes: TLabel
      Left = 4
      Top = 13
      Width = 88
      Height = 13
      Caption = 'Highlighter theme:'
      FocusControl = cbThemes
    end
    object lblBrushes: TLabel
      Left = 4
      Top = 184
      Width = 101
      Height = 13
      Caption = 'Source code "brush":'
      FocusControl = cbBrushes
    end
    object cbThemes: TComboBox
      Left = 118
      Top = 10
      Width = 156
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbThemesChange
    end
    object cbBrushes: TComboBox
      Left = 118
      Top = 181
      Width = 219
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = cbBrushesChange
    end
    object gbThemeProps: TGroupBox
      Left = 0
      Top = 78
      Width = 594
      Height = 92
      Caption = 'Theme properties'
      TabOrder = 5
      object lblFontName: TLabel
        Left = 8
        Top = 26
        Width = 55
        Height = 13
        Caption = 'Font name:'
        FocusControl = cbFontName
      end
      object lblDefForeground: TLabel
        Left = 314
        Top = 24
        Width = 128
        Height = 13
        Caption = 'Default foreground colour:'
      end
      object lblDefBackground: TLabel
        Left = 314
        Top = 58
        Width = 130
        Height = 13
        Caption = 'Default background colour:'
      end
      object lblFontSize: TLabel
        Left = 8
        Top = 58
        Width = 47
        Height = 13
        Caption = 'Font size:'
        FocusControl = cbFontSize
      end
      object cbFontName: TComboBox
        Left = 74
        Top = 22
        Width = 197
        Height = 21
        Style = csDropDownList
        Sorted = True
        TabOrder = 0
        OnChange = cbFontNameChange
      end
      object cbFontSize: TComboBox
        Left = 74
        Top = 54
        Width = 72
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = cbFontSizeChange
        OnKeyPress = cbFontSizeKeyPress
      end
    end
    object btnDelete: TButton
      Left = 518
      Top = 8
      Width = 75
      Height = 25
      Action = actDelete
      TabOrder = 2
    end
    object btnSave: TButton
      Left = 518
      Top = 39
      Width = 75
      Height = 25
      Action = actSave
      TabOrder = 4
    end
    object gbElements: TGroupBox
      Left = 0
      Top = 214
      Width = 594
      Height = 226
      Caption = 'Element style'
      TabOrder = 8
      object lblElements: TLabel
        Left = 8
        Top = 20
        Width = 47
        Height = 13
        Caption = '&Elements:'
        FocusControl = lbElements
      end
      object lblForeground: TLabel
        Left = 341
        Top = 20
        Width = 94
        Height = 13
        Caption = 'Foreground Colour:'
      end
      object lblExample: TLabel
        Left = 206
        Top = 144
        Width = 44
        Height = 13
        Caption = 'Example:'
      end
      object lblBackground: TLabel
        Left = 341
        Top = 60
        Width = 94
        Height = 13
        Caption = 'Background Colour:'
      end
      object lbElements: TListBox
        Left = 8
        Top = 37
        Width = 173
        Height = 181
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbElementsClick
      end
      object gbFontStyle: TGroupBox
        Left = 206
        Top = 20
        Width = 111
        Height = 114
        Caption = ' Font style '
        TabOrder = 1
        object chkBold: TCheckBox
          Left = 8
          Top = 40
          Width = 89
          Height = 17
          Caption = '&Bold'
          TabOrder = 1
          OnClick = FontStyleCheckBoxClick
        end
        object chkItalics: TCheckBox
          Left = 8
          Top = 62
          Width = 89
          Height = 17
          Caption = '&Italics'
          TabOrder = 2
          OnClick = FontStyleCheckBoxClick
        end
        object chkUnderline: TCheckBox
          Left = 8
          Top = 84
          Width = 89
          Height = 21
          Caption = '&Underline'
          TabOrder = 3
          OnClick = FontStyleCheckBoxClick
        end
        object chkDefaultFS: TCheckBox
          Left = 8
          Top = 18
          Width = 89
          Height = 17
          Caption = 'Default'
          TabOrder = 0
          OnClick = FontStyleCheckBoxClick
        end
      end
      object btnDefaultStyle: TButton
        Left = 424
        Top = 88
        Width = 155
        Height = 25
        Action = actDefaultStyle
        TabOrder = 2
      end
      inline frmExample: THiliteThemesExampleFrame
        Left = 206
        Top = 163
        Width = 323
        Height = 55
        TabOrder = 3
        ExplicitLeft = 206
        ExplicitTop = 163
        ExplicitWidth = 323
        ExplicitHeight = 55
        inherited pnlBrowser: TPanel
          Width = 323
          Height = 55
          ExplicitWidth = 323
          ExplicitHeight = 55
          inherited wbBrowser: TWebBrowser
            Width = 323
            Height = 55
            ExplicitWidth = 374
            ExplicitHeight = 55
            ControlData = {
              4C00000062210000AF0500000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
    object btnPreview: TButton
      Left = 358
      Top = 179
      Width = 75
      Height = 25
      Action = actPreview
      TabOrder = 7
    end
    object btnNew: TButton
      Left = 437
      Top = 8
      Width = 75
      Height = 25
      Action = actNew
      TabOrder = 1
    end
    object btnUpdate: TButton
      Left = 437
      Top = 39
      Width = 75
      Height = 25
      Action = actUpdate
      TabOrder = 3
    end
  end
  inherited btnHelp: TButton
    Top = 454
    ExplicitTop = 454
  end
  inherited btnClose: TButton
    Top = 454
    ExplicitTop = 454
  end
  object alDlg: TActionList
    Left = 408
    Top = 440
    object actDelete: TAction
      Caption = 'Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actSave: TAction
      Caption = 'Save As...'
      OnExecute = actSaveExecute
    end
    object actPreview: TAction
      Caption = 'Preview...'
      OnExecute = actPreviewExecute
      OnUpdate = actPreviewUpdate
    end
    object actNew: TAction
      Caption = 'New'
      OnExecute = actNewExecute
    end
    object actDefaultStyle: TAction
      Caption = 'Use Default Style'
      OnExecute = actDefaultStyleExecute
      OnUpdate = actDefaultStyleUpdate
    end
    object actUpdate: TAction
      Caption = 'Update'
      OnExecute = actUpdateExecute
      OnUpdate = actUpdateUpdate
    end
  end
end
