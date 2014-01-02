inherited SnippetsEditorDlg: TSnippetsEditorDlg
  Left = 1097
  Top = 525
  Caption = 'SnippetsEditorDlg'
  ClientHeight = 578
  ClientWidth = 738
  Position = poDesigned
  ExplicitWidth = 744
  ExplicitHeight = 606
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
          Top = 47
          Width = 57
          Height = 13
          Caption = '&Description:'
          FocusControl = frmDescription
        end
        object lblSourceCode: TLabel
          Left = 0
          Top = 178
          Width = 63
          Height = 13
          Caption = '&Source code:'
          FocusControl = frmSourceEditor
        end
        object lblLanguages: TLabel
          Left = 0
          Top = 157
          Width = 51
          Height = 13
          Caption = '&Language:'
          FocusControl = cbLanguages
        end
        object lblSnippetKindHelp: TLabel
          Left = 330
          Top = 120
          Width = 117
          Height = 13
          Cursor = crHandPoint
          Caption = 'What are Snippet Kinds?'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = lblSnippetKindHelpClick
        end
        object lblKind: TLabel
          Left = 0
          Top = 125
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = cbKind
        end
        object lblTitle: TLabel
          Left = 3
          Top = 14
          Width = 24
          Height = 13
          Caption = '&Title:'
          FocusControl = edTitle
        end
        object cbLanguages: TComboBox
          Left = 93
          Top = 153
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 4
          OnChange = cbLanguagesChange
        end
        object cbKind: TComboBox
          Left = 93
          Top = 120
          Width = 209
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = cbKindChange
        end
        inline frmDescription: TSnippetsActiveTextEdFrame
          Left = 93
          Top = 41
          Width = 462
          Height = 78
          Color = clWindow
          ParentBackground = False
          ParentColor = False
          TabOrder = 1
          ExplicitLeft = 93
          ExplicitTop = 41
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
          Top = 39
          Width = 85
          Height = 25
          Action = actViewDescription
          Caption = 'Previe&w...'
          TabOrder = 2
        end
        object edTitle: TEdit
          Left = 93
          Top = 8
          Width = 298
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 0
        end
        inline frmSourceEditor: TCodeEditorFrame
          Left = 0
          Top = 194
          Width = 652
          Height = 250
          TabOrder = 5
          ExplicitTop = 194
          ExplicitWidth = 652
          ExplicitHeight = 250
        end
      end
      object tsReferences: TTabSheet
        Caption = 'References'
        ImageIndex = 1
        object lblXRefs: TLabel
          Left = 3
          Top = 3
          Width = 87
          Height = 13
          Caption = 'Cross-&references:'
          FocusControl = clbXRefs
        end
        object lblDepends: TLabel
          Left = 228
          Top = 3
          Width = 71
          Height = 13
          Caption = 'De&pendencies:'
          FocusControl = clbDepends
        end
        object lblUnits: TLabel
          Left = 452
          Top = 3
          Width = 28
          Height = 13
          Caption = '&Units:'
          FocusControl = clbUnits
        end
        object clbXRefs: TCheckListBox
          Left = 3
          Top = 22
          Width = 217
          Height = 420
          IntegralHeight = True
          ItemHeight = 13
          PopupMenu = mnuXRefs
          TabOrder = 0
        end
        object clbDepends: TCheckListBox
          Left = 228
          Top = 22
          Width = 217
          Height = 420
          IntegralHeight = True
          ItemHeight = 13
          PopupMenu = mnuDependencies
          TabOrder = 1
        end
        object clbUnits: TCheckListBox
          Left = 453
          Top = 22
          Width = 197
          Height = 420
          IntegralHeight = True
          ItemHeight = 13
          PopupMenu = mnuUnits
          TabOrder = 2
        end
        object edUnit: TEdit
          Left = 452
          Top = 441
          Width = 117
          Height = 21
          PopupMenu = mnuEditCtrls
          TabOrder = 3
        end
        object btnAddUnit: TButton
          Left = 575
          Top = 441
          Width = 75
          Height = 25
          Action = actAddUnit
          TabOrder = 4
        end
      end
      object tsComments: TTabSheet
        Caption = 'Notes'
        ImageIndex = 2
        object lblNotes: TLabel
          Left = 3
          Top = 3
          Width = 32
          Height = 13
          Caption = '&Notes:'
          FocusControl = frmNotes
        end
        object lblNotesCaretPos: TLabel
          Left = 560
          Top = 3
          Width = 91
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblNotesCaretPos'
        end
        object btnViewNotes: TButton
          Left = 3
          Top = 409
          Width = 85
          Height = 25
          Action = actViewNotes
          TabOrder = 1
        end
        inline frmNotes: TSnippetsActiveTextEdFrame
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
          Width = 93
          Height = 13
          Caption = 'Set &compile results:'
          FocusControl = lbCompilers
        end
        object lblCompileShortcuts: TLabel
          Left = 203
          Top = 3
          Width = 46
          Height = 13
          Caption = 'Shortcuts'
        end
        object lblCompResDesc: TLabel
          Left = 3
          Top = 405
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
          Height = 373
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
            Font.Name = 'Tahoma'
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
    object actClearUnits: TAction
      Caption = 'Deselect All Units'
      OnExecute = actClearUnitsExecute
      OnUpdate = actClearUnitsUpdate
    end
    object actDeleteUnit: TAction
      Caption = 'Delete Selected Unit'
      OnExecute = actDeleteUnitExecute
      OnUpdate = actDeleteUnitUpdate
    end
    object actRestoreUnits: TAction
      Caption = 'Restore Default Units'
      OnExecute = actRestoreUnitsExecute
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
    object actViewDependencies: TAction
      Caption = 'View &Dependencies...'
      OnExecute = actViewDependenciesExecute
    end
    object actClearDependencies: TAction
      Caption = 'Deselect All Dependencies'
      OnExecute = actClearDependenciesExecute
      OnUpdate = actClearDependenciesUpdate
    end
    object actClearXRefs: TAction
      Caption = 'Deselect All Cross-References'
      OnExecute = actClearXRefsExecute
      OnUpdate = actClearXRefsUpdate
    end
    object actViewNotes: TAction
      Caption = 'Previe&w...'
      OnExecute = actViewNotesExecute
      OnUpdate = actViewNotesUpdate
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
  object mnuDependencies: TPopupMenu
    Left = 264
    Top = 344
    object miClearDependencies: TMenuItem
      Action = actClearDependencies
    end
    object miViewDependencies: TMenuItem
      Action = actViewDependencies
    end
  end
  object mnuXRefs: TPopupMenu
    Left = 80
    Top = 336
    object miClearXRefs: TMenuItem
      Action = actClearXRefs
    end
  end
  object mnuUnits: TPopupMenu
    Left = 496
    Top = 344
    object miClearUnits: TMenuItem
      Action = actClearUnits
    end
    object miSpacer3: TMenuItem
      Caption = '-'
    end
    object miDeleteUnit: TMenuItem
      Action = actDeleteUnit
    end
    object miRestoreUnits: TMenuItem
      Action = actRestoreUnits
    end
  end
end
