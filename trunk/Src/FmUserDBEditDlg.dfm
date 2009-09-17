inherited UserDBEditDlg: TUserDBEditDlg
  Left = 1097
  Top = 525
  Caption = 'UserDBEditDlg'
  ClientHeight = 578
  ClientWidth = 738
  Position = poDesigned
  ExplicitWidth = 744
  ExplicitHeight = 604
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 646
    Height = 465
    ExplicitWidth = 646
    ExplicitHeight = 465
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 646
      Height = 465
      ActivePage = tsCompileResults
      Align = alClient
      TabOrder = 0
      OnChange = pcMainChange
      object tsCode: TTabSheet
        Caption = 'Code'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblDescription: TLabel
          Left = 3
          Top = 343
          Width = 56
          Height = 13
          Caption = '&Description:'
          FocusControl = edDescription
        end
        object lblSourceCode: TLabel
          Left = 3
          Top = 3
          Width = 64
          Height = 13
          Caption = '&Source code:'
          FocusControl = edSourceCode
        end
        object lblName: TLabel
          Left = 3
          Top = 369
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = edName
        end
        object lblCategories: TLabel
          Left = 3
          Top = 397
          Width = 45
          Height = 13
          Caption = '&Category:'
          FocusControl = cbCategories
        end
        object lblSnippetKindHelp: TLabel
          Left = 236
          Top = 311
          Width = 118
          Height = 13
          Cursor = crHandPoint
          Caption = 'What are Snippet Kinds?'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = lblSnippetKindHelpClick
        end
        object lblKind: TLabel
          Left = 3
          Top = 316
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = cbKind
        end
        object edSourceCode: TMemo
          Left = 3
          Top = 22
          Width = 631
          Height = 283
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object edDescription: TEdit
          Left = 80
          Top = 339
          Width = 554
          Height = 21
          TabOrder = 2
        end
        object edName: TEdit
          Left = 80
          Top = 366
          Width = 147
          Height = 21
          TabOrder = 3
        end
        object cbCategories: TComboBox
          Left = 80
          Top = 393
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 4
        end
        object cbKind: TComboBox
          Left = 80
          Top = 312
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
          OnChange = cbKindChange
        end
      end
      object tsReferences: TTabSheet
        Caption = 'References'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblXRefs: TLabel
          Left = 3
          Top = 3
          Width = 82
          Height = 13
          Caption = 'Cross-&references:'
          FocusControl = clbXRefs
        end
        object lblDepends: TLabel
          Left = 219
          Top = 3
          Width = 72
          Height = 13
          Caption = 'De&pendencies:'
          FocusControl = clbDepends
        end
        object lblUnits: TLabel
          Left = 435
          Top = 3
          Width = 27
          Height = 13
          Caption = '&Units:'
          FocusControl = clbUnits
        end
        object clbXRefs: TCheckListBox
          Left = 3
          Top = 22
          Width = 209
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 0
        end
        object clbDepends: TCheckListBox
          Left = 219
          Top = 22
          Width = 209
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 1
        end
        object clbUnits: TCheckListBox
          Left = 435
          Top = 22
          Width = 198
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          Items.Strings = (
            'SysUtils'
            'Classes'
            'Controls'
            'Messages'
            'Windows'
            'Graphics'
            'ShlObj'
            'ShellAPI'
            'ActiveX')
          TabOrder = 3
        end
        object edUnit: TEdit
          Left = 435
          Top = 398
          Width = 117
          Height = 21
          TabOrder = 4
        end
        object btnAddUnit: TButton
          Left = 558
          Top = 396
          Width = 75
          Height = 25
          Action = actAddUnit
          TabOrder = 5
        end
        object btnDependencies: TButton
          Left = 219
          Top = 409
          Width = 141
          Height = 25
          Action = actDependencies
          TabOrder = 2
        end
      end
      object tsComments: TTabSheet
        Caption = 'Extra Information'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblExtra: TLabel
          Left = 3
          Top = 3
          Width = 81
          Height = 13
          Caption = 'E&xtra information:'
          FocusControl = edExtra
        end
        object edExtra: TMemo
          Left = 3
          Top = 22
          Width = 631
          Height = 147
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
        inline frmExtraInstructions: TFixedHTMLDlgFrame
          Left = 3
          Top = 172
          Width = 631
          Height = 122
          TabOrder = 1
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 172
          ExplicitWidth = 631
          ExplicitHeight = 122
          inherited pnlBrowser: TPanel
            Width = 631
            Height = 122
            ExplicitWidth = 631
            ExplicitHeight = 122
            inherited wbBrowser: TWebBrowser
              Width = 631
              Height = 122
              ExplicitTop = 1
              ExplicitWidth = 631
              ExplicitHeight = 122
              ControlData = {
                4C000000374100009C0C00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
        object btnViewExtra: TButton
          Left = 3
          Top = 409
          Width = 166
          Height = 25
          Action = actViewExtra
          TabOrder = 2
        end
      end
      object tsCompileResults: TTabSheet
        Caption = 'Compile Results'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblCompilers: TLabel
          Left = 3
          Top = 3
          Width = 91
          Height = 13
          Caption = 'Set &compile results:'
          FocusControl = lbCompilers
        end
        object lblCompileShortcuts: TLabel
          Left = 203
          Top = 3
          Width = 45
          Height = 13
          Caption = 'Shortcuts'
        end
        object lblCompResDesc: TLabel
          Left = 3
          Top = 349
          Width = 166
          Height = 60
          AutoSize = False
          Caption = 
            'Use the a drop down buttons to display a list of compile results' +
            '. Pressing space activates the selected button.'
          WordWrap = True
        end
        object lbCompilers: TListBox
          Left = 3
          Top = 22
          Width = 166
          Height = 312
          Style = lbOwnerDrawFixed
          ItemHeight = 28
          TabOrder = 0
        end
        object btnSetAllSuccess: TBitBtn
          Left = 203
          Top = 22
          Width = 141
          Height = 25
          Caption = 'btnSetAllSuccess'
          TabOrder = 1
          OnClick = actSetAllSuccessExecute
        end
        object btnCompile: TButton
          Left = 203
          Top = 84
          Width = 140
          Height = 25
          Action = actCompile
          TabOrder = 3
        end
        object btnSetAllQuery: TBitBtn
          Left = 203
          Top = 53
          Width = 141
          Height = 25
          Action = actSetAllQuery
          Caption = 'Set All To &Query'
          TabOrder = 2
        end
        object pnlViewCompErrs: TPanel
          Left = 203
          Top = 115
          Width = 238
          Height = 72
          BevelOuter = bvNone
          TabOrder = 4
          object lblViewCompErrsKey: TLabel
            Left = 0
            Top = 27
            Width = 96
            Height = 13
            Caption = 'lblViewCompErrsKey'
          end
          object lblViewCompErrs: TLabel
            Left = 0
            Top = 8
            Width = 78
            Height = 13
            Cursor = crHandPoint
            Caption = 'lblViewCompErrs'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsUnderline]
            ParentFont = False
            OnClick = lblViewCompErrsClick
          end
        end
      end
    end
  end
  inherited btnHelp: TButton
    Left = 328
    Top = 536
    ExplicitLeft = 328
    ExplicitTop = 536
  end
  inherited btnCancel: TButton
    Left = 248
    Top = 536
    ExplicitLeft = 248
    ExplicitTop = 536
  end
  inherited btnOK: TButton
    Left = 169
    Top = 536
    OnClick = btnOKClick
    ExplicitLeft = 169
    ExplicitTop = 536
  end
  object alMain: TActionList
    Left = 48
    Top = 488
    object actAddUnit: TAction
      Caption = '&Add Unit'
      OnExecute = actAddUnitExecute
      OnUpdate = actAddUnitUpdate
    end
    object actCompile: TAction
      Caption = '&Test Compile'
      ShortCut = 120
      OnExecute = actCompileExecute
      OnUpdate = actCompileUpdate
    end
    object actSetAllQuery: TAction
      Caption = 'Set All To &Query'
      ImageIndex = 3
      OnExecute = actSetAllQueryExecute
    end
    object actSetAllSuccess: TAction
      Caption = 'Set All To &Success'
      ImageIndex = 0
      OnExecute = actSetAllSuccessExecute
    end
    object actViewErrors: TAction
      Caption = 'View Errors and Warnings...'
      ShortCut = 32854
      OnExecute = actViewErrorsExecute
      OnUpdate = actViewErrorsUpdate
    end
    object actDependencies: TAction
      Caption = 'View Dependencies...'
      OnExecute = actDependenciesExecute
    end
    object actViewExtra: TAction
      Caption = 'Render Extra Information...'
      OnExecute = actViewExtraExecute
      OnUpdate = actViewExtraUpdate
    end
  end
end
