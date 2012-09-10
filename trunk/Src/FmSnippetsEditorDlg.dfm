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
    Width = 662
    Height = 504
    ExplicitWidth = 662
    ExplicitHeight = 504
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 662
      Height = 504
      ActivePage = tsCode
      Align = alClient
      TabOrder = 0
      OnChange = pcMainChange
      OnMouseDown = pcMainMouseDown
      object tsCode: TTabSheet
        Caption = 'Code'
        object lblDescription: TLabel
          Left = 0
          Top = 73
          Width = 56
          Height = 13
          Caption = '&Description:'
        end
        object lblSourceCode: TLabel
          Left = 0
          Top = 204
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
          Left = 0
          Top = 183
          Width = 45
          Height = 13
          Caption = '&Category:'
          FocusControl = cbCategories
        end
        object lblSnippetKindHelp: TLabel
          Left = 330
          Top = 146
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
          Left = 0
          Top = 151
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = cbKind
        end
        object lblSourceCaretPos: TLabel
          Left = 556
          Top = 204
          Width = 91
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblSourceCaretPos'
        end
        object lblDisplayName: TLabel
          Left = 3
          Top = 40
          Width = 68
          Height = 13
          Caption = 'Displa&y Name:'
          FocusControl = edDisplayName
        end
        object edSourceCode: TMemo
          Left = 4
          Top = 224
          Width = 647
          Height = 225
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = mnuEditCtrls
          ScrollBars = ssBoth
          TabOrder = 6
        end
        object edName: TEdit
          Left = 93
          Top = 7
          Width = 209
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 0
        end
        object cbCategories: TComboBox
          Left = 93
          Top = 179
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 5
        end
        object cbKind: TComboBox
          Left = 93
          Top = 146
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 4
          OnChange = cbKindChange
        end
        inline frmDescription: TSnippetsActiveTextEdFrame
          Left = 93
          Top = 67
          Width = 462
          Height = 78
          Color = clWindow
          ParentBackground = False
          ParentColor = False
          TabOrder = 2
          ExplicitLeft = 93
          ExplicitTop = 67
          ExplicitWidth = 462
          ExplicitHeight = 78
          inherited edText: TMemo
            Width = 462
            Height = 55
            ExplicitWidth = 462
            ExplicitHeight = 55
          end
          inherited tcEditMode: TTabControl
            Top = 55
            Width = 462
            ExplicitTop = 55
            ExplicitWidth = 462
          end
        end
        object btnViewDescription: TButton
          Left = 562
          Top = 65
          Width = 85
          Height = 25
          Action = actViewDescription
          TabOrder = 3
        end
        object edDisplayName: TEdit
          Left = 93
          Top = 34
          Width = 298
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 1
        end
        object chkUseHiliter: TCheckBox
          Left = 3
          Top = 455
          Width = 478
          Height = 17
          Caption = 'Synta&x highlight this snippet as Pascal code'
          TabOrder = 7
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
          Left = 228
          Top = 3
          Width = 72
          Height = 13
          Caption = 'De&pendencies:'
          FocusControl = clbDepends
        end
        object lblUnits: TLabel
          Left = 452
          Top = 3
          Width = 27
          Height = 13
          Caption = '&Units:'
          FocusControl = clbUnits
        end
        object clbXRefs: TCheckListBox
          Left = 3
          Top = 22
          Width = 217
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 0
        end
        object clbDepends: TCheckListBox
          Left = 228
          Top = 22
          Width = 217
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 1
        end
        object clbUnits: TCheckListBox
          Left = 451
          Top = 24
          Width = 198
          Height = 368
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 3
        end
        object edUnit: TEdit
          Left = 452
          Top = 398
          Width = 117
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 4
        end
        object btnAddUnit: TButton
          Left = 575
          Top = 396
          Width = 75
          Height = 25
          Action = actAddUnit
          TabOrder = 5
        end
        object btnDependencies: TButton
          Left = 228
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
        end
        object lblExtraCaretPos: TLabel
          Left = 560
          Top = 3
          Width = 91
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblExtraCaretPos'
        end
        object btnViewExtra: TButton
          Left = 3
          Top = 409
          Width = 85
          Height = 25
          Action = actViewExtra
          TabOrder = 1
        end
        inline frmExtra: TSnippetsActiveTextEdFrame
          Left = 3
          Top = 22
          Width = 648
          Height = 381
          Color = clWindow
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
          ExplicitLeft = 3
          ExplicitTop = 22
          ExplicitWidth = 648
          ExplicitHeight = 381
          inherited edText: TMemo
            Width = 648
            Height = 358
            ExplicitWidth = 648
            ExplicitHeight = 358
          end
          inherited tcEditMode: TTabControl
            Top = 358
            Width = 648
            ExplicitTop = 358
            ExplicitWidth = 648
          end
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
        object btnSetAllSuccess: TButton
          Left = 203
          Top = 22
          Width = 141
          Height = 25
          Action = actSetAllSuccess
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
        end
        object btnCompile: TButton
          Left = 204
          Top = 84
          Width = 140
          Height = 25
          Action = actCompile
          TabOrder = 3
        end
        object btnSetAllQuery: TButton
          Left = 203
          Top = 53
          Width = 141
          Height = 25
          Action = actSetAllQuery
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
    Left = 146
    Top = 264
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
      Caption = 'Preview...'
      OnExecute = actViewExtraExecute
      OnUpdate = actViewExtraUpdate
    end
    object actCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ShortCut = 16472
    end
    object actCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ShortCut = 16451
    end
    object actPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
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
      ShortCut = 16474
    end
    object actViewTestUnit: TAction
      Caption = 'View Test &Unit...'
      OnExecute = actViewTestUnitExecute
      OnUpdate = actViewTestUnitUpdate
    end
    object actViewDescription: TAction
      Caption = 'Pre&view...'
      OnExecute = actViewDescriptionExecute
      OnUpdate = actViewDescriptionUpdate
    end
  end
  object mnuEditCtrls: TPopupMenu
    Left = 86
    Top = 264
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
end
