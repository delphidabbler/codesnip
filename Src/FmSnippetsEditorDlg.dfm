inherited SnippetsEditorDlg: TSnippetsEditorDlg
  Left = 1097
  Top = 525
  Caption = 'SnippetsEditorDlg'
  ClientHeight = 578
  ClientWidth = 738
  Position = poDesigned
  ExplicitWidth = 744
  ExplicitHeight = 604
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 646
    Height = 504
    ExplicitWidth = 646
    ExplicitHeight = 504
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 646
      Height = 504
      ActivePage = tsCode
      Align = alClient
      TabOrder = 0
      OnChange = pcMainChange
      OnMouseDown = pcMainMouseDown
      object tsCode: TTabSheet
        Caption = 'Code'
        object lblDescription: TLabel
          Left = 3
          Top = 41
          Width = 56
          Height = 13
          Caption = '&Description:'
        end
        object lblSourceCode: TLabel
          Left = 3
          Top = 172
          Width = 64
          Height = 13
          Caption = '&Source code:'
          FocusControl = edSourceCode
        end
        object lblName: TLabel
          Left = 3
          Top = 11
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = edName
        end
        object lblCategories: TLabel
          Left = 3
          Top = 143
          Width = 45
          Height = 13
          Caption = '&Category:'
          FocusControl = cbCategories
        end
        object lblSnippetKindHelp: TLabel
          Left = 333
          Top = 106
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
          Top = 111
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = cbKind
        end
        object lblSourceCaretPos: TLabel
          Left = 543
          Top = 172
          Width = 91
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblSourceCaretPos'
        end
        object edSourceCode: TMemo
          Left = 3
          Top = 191
          Width = 631
          Height = 283
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = mnuEditCtrls
          ScrollBars = ssBoth
          TabOrder = 5
        end
        object edName: TEdit
          Left = 80
          Top = 8
          Width = 209
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 0
        end
        object cbCategories: TComboBox
          Left = 80
          Top = 139
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 4
        end
        object cbKind: TComboBox
          Left = 80
          Top = 106
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = cbKindChange
        end
        inline frmDescription: TSnippetsActiveTextEdFrame
          Left = 80
          Top = 35
          Width = 462
          Height = 78
          Color = clWindow
          ParentBackground = False
          ParentColor = False
          TabOrder = 1
          ExplicitLeft = 80
          ExplicitTop = 35
          ExplicitWidth = 462
          ExplicitHeight = 78
          inherited edText: TMemo
            Width = 462
            Height = 55
            ExplicitTop = 23
            ExplicitWidth = 462
            ExplicitHeight = 47
          end
          inherited tcEditMode: TTabControl
            Top = 55
            Width = 462
            ExplicitWidth = 462
          end
        end
        object btnViewDescription: TButton
          Left = 549
          Top = 33
          Width = 85
          Height = 25
          Action = actViewDescription
          TabOrder = 2
        end
      end
      object tsReferences: TTabSheet
        Caption = 'References'
        ImageIndex = 1
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
        object lblExtra: TLabel
          Left = 3
          Top = 3
          Width = 81
          Height = 13
          Caption = 'E&xtra information:'
          FocusControl = edExtra
        end
        object lblExtraCaretPos: TLabel
          Left = 544
          Top = 3
          Width = 91
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblExtraCaretPos'
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
          PopupMenu = mnuEditCtrls
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
          Action = actSetAllSuccess
          Caption = 'Set All To &Success'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
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
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
        end
        object pnlViewCompErrs: TPanel
          Left = 203
          Top = 115
          Width = 238
          Height = 72
          BevelOuter = bvNone
          TabOrder = 5
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
        object btnViewTestUnit: TButton
          Left = 350
          Top = 84
          Width = 140
          Height = 25
          Action = actViewTestUnit
          TabOrder = 4
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
    Images = ilMain
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
      OnExecute = actSetAllQueryExecute
    end
    object actSetAllSuccess: TAction
      Caption = 'Set All To &Success'
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
    object actCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object actCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object actPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object actSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object actUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object actViewTestUnit: TAction
      Caption = 'View Test &Unit...'
      OnExecute = actViewTestUnitExecute
      OnUpdate = actViewTestUnitUpdate
    end
    object actViewDescription: TAction
      Caption = 'Preview...'
      OnExecute = actViewDescriptionExecute
      OnUpdate = actViewDescriptionUpdate
    end
  end
  object mnuEditCtrls: TPopupMenu
    Images = ilMain
    Left = 80
    Top = 488
    object miCut: TMenuItem
      Action = actCut
    end
    object miCopy: TMenuItem
      Action = actCopy
    end
    object miPaste: TMenuItem
      Action = actPaste
    end
    object miSpacer1: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Action = actSelectAll
    end
    object miSpacer2: TMenuItem
      Caption = '-'
    end
    object miUndo: TMenuItem
      Action = actUndo
    end
  end
  object ilMain: TImageList
    Left = 112
    Top = 488
    Bitmap = {
      494C010104000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00000000000000000000000000FFFF
      FF00840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000644904006449
      0400644904006449040064490400000000000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000916B0A007F5B
      00007F5B0000916B0A0000000000000000000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFF
      FF00840000008400000084000000840000000000000000848400848484000084
      8400848484000084840084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000916B0A007F5B
      0000916B0A00D9A77D0000000000000000000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484000000000000000000000000000000000000000000916B0A00916B
      0A00D9A77D007F5B0000D9A77D00000000000000000000000000000000000000
      0000D9A77D007F5B000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000916B0A000000
      000000000000D9A77D007F5B00007F5B0000D9A77D000000000000000000D9A7
      7D007F5B0000D9A77D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D9A77D007F5B00007F5B00007F5B00007F5B
      0000D9A77D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400848484000084
      84000000000000FFFF00000000000000000000FFFF0000000000848484000084
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFF9FFFFFFFC00FFFF
      F6CFFE008000FFFFF6B7FE000000FFFFF6B7FE000000FFFFF8B780000000FFFF
      FE8F80000001C1FBFE3F80000003C3FBFF7F80000003C3FBFE3F80010003C1F3
      FEBF80030003D863FC9F80070FC3FE07FDDF807F0003FFFFFDDF80FF8007FFFF
      FDDF81FFF87FFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
