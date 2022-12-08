inherited SnippetsEditorDlg: TSnippetsEditorDlg
  Caption = 'SnippetsEditorDlg'
  ClientHeight = 946
  ClientWidth = 1359
  ExplicitLeft = 4
  ExplicitTop = 4
  ExplicitWidth = 1371
  ExplicitHeight = 1008
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Width = 1149
    Height = 922
    ExplicitWidth = 1149
    ExplicitHeight = 922
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 1149
      Height = 922
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsCompileResults
      Align = alClient
      TabOrder = 0
      OnChange = pcMainChange
      OnMouseDown = pcMainMouseDown
      ExplicitWidth = 957
      ExplicitHeight = 631
      object tsCode: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Code'
        object lblDescription: TLabel
          Left = 0
          Top = 128
          Width = 110
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Description:'
          FocusControl = frmDescription
        end
        object lblSourceCode: TLabel
          Left = 0
          Top = 357
          Width = 119
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Source code:'
          FocusControl = edSourceCode
        end
        object lblName: TLabel
          Left = 5
          Top = 19
          Width = 61
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Name:'
          FocusControl = edName
        end
        object lblCategories: TLabel
          Left = 0
          Top = 320
          Width = 88
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Category:'
          FocusControl = cbCategories
        end
        object lblSnippetKindHelp: TLabel
          Left = 578
          Top = 256
          Width = 226
          Height = 30
          Cursor = crHandPoint
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'What are Snippet Kinds?'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = lblSnippetKindHelpClick
        end
        object lblKind: TLabel
          Left = 0
          Top = 264
          Width = 46
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Kind:'
          FocusControl = cbKind
        end
        object lblSourceCaretPos: TLabel
          Left = 973
          Top = 357
          Width = 159
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblSourceCaretPos'
        end
        object lblDisplayName: TLabel
          Left = 5
          Top = 70
          Width = 134
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Displa&y Name:'
          FocusControl = edDisplayName
        end
        object edSourceCode: TMemo
          Left = 7
          Top = 392
          Width = 1132
          Height = 394
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = mnuEditCtrls
          ScrollBars = ssBoth
          TabOrder = 6
        end
        object edName: TEdit
          Left = 163
          Top = 12
          Width = 366
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          PopupMenu = mnuEditCtrls
          TabOrder = 0
        end
        object cbCategories: TComboBox
          Left = 163
          Top = 313
          Width = 366
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Style = csDropDownList
          TabOrder = 5
        end
        object cbKind: TComboBox
          Left = 163
          Top = 256
          Width = 366
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Style = csDropDownList
          TabOrder = 4
          OnChange = cbKindChange
        end
        inline frmDescription: TSnippetsActiveTextEdFrame
          Left = 93
          Top = 67
          Width = 462
          Height = 78
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
            Height = 38
            ExplicitWidth = 462
            ExplicitHeight = 38
          end
          inherited tcEditMode: TTabControl
            Top = 38
            Width = 462
            ExplicitTop = 38
            ExplicitWidth = 462
          end
        end
        object btnViewDescription: TButton
          Left = 984
          Top = 114
          Width = 148
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actViewDescription
          Caption = 'Previe&w...'
          TabOrder = 3
        end
        object edDisplayName: TEdit
          Left = 163
          Top = 60
          Width = 521
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          PopupMenu = mnuEditCtrls
          TabOrder = 1
        end
        object chkUseHiliter: TCheckBox
          Left = 5
          Top = 796
          Width = 837
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Synta&x highlight this snippet as Pascal code'
          TabOrder = 7
        end
      end
      object tsReferences: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'References'
        ImageIndex = 1
        object lblXRefs: TLabel
          Left = 5
          Top = 5
          Width = 159
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Cross-&references:'
          FocusControl = clbXRefs
        end
        object lblDepends: TLabel
          Left = 399
          Top = 5
          Width = 136
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'De&pendencies:'
          FocusControl = clbDepends
        end
        object lblUnits: TLabel
          Left = 791
          Top = 5
          Width = 52
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Units:'
          FocusControl = clbUnits
        end
        object clbXRefs: TCheckListBox
          Left = 5
          Top = 39
          Width = 380
          Height = 694
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          IntegralHeight = True
          ItemHeight = 30
          PopupMenu = mnuXRefs
          TabOrder = 0
        end
        object clbDepends: TCheckListBox
          Left = 399
          Top = 39
          Width = 380
          Height = 694
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          IntegralHeight = True
          ItemHeight = 30
          PopupMenu = mnuDependencies
          TabOrder = 1
        end
        object clbUnits: TCheckListBox
          Left = 793
          Top = 39
          Width = 345
          Height = 694
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          IntegralHeight = True
          ItemHeight = 30
          PopupMenu = mnuUnits
          TabOrder = 2
        end
        object edUnit: TEdit
          Left = 791
          Top = 772
          Width = 205
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          PopupMenu = mnuEditCtrls
          TabOrder = 3
        end
        object btnAddUnit: TButton
          Left = 1006
          Top = 772
          Width = 132
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actAddUnit
          TabOrder = 4
        end
      end
      object tsComments: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Extra Information'
        ImageIndex = 2
        object lblExtra: TLabel
          Left = 5
          Top = 5
          Width = 165
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'E&xtra information:'
          FocusControl = frmExtra
        end
        object lblExtraCaretPos: TLabel
          Left = 980
          Top = 5
          Width = 159
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblExtraCaretPos'
        end
        object btnViewExtra: TButton
          Left = 5
          Top = 716
          Width = 149
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actViewExtra
          TabOrder = 1
        end
        inline frmExtra: TSnippetsActiveTextEdFrame
          Left = 3
          Top = 22
          Width = 648
          Height = 381
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
            Height = 341
            ExplicitWidth = 648
            ExplicitHeight = 341
          end
          inherited tcEditMode: TTabControl
            Top = 341
            Width = 648
            ExplicitTop = 341
            ExplicitWidth = 648
          end
        end
      end
      object tsCompileResults: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Compile Results'
        ImageIndex = 3
        object lblCompilers: TLabel
          Left = 5
          Top = 5
          Width = 179
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Set &compile results:'
          FocusControl = lbCompilers
        end
        object lblCompileShortcuts: TLabel
          Left = 355
          Top = 5
          Width = 87
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Shortcuts'
        end
        object lblCompResDesc: TLabel
          Left = 5
          Top = 702
          Width = 326
          Height = 105
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'Use the a drop down buttons to display a list of compile results' +
            '. Pressing space activates the selected button.'
          WordWrap = True
        end
        object lbCompilers: TListBox
          Left = 5
          Top = 39
          Width = 326
          Height = 652
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Style = lbOwnerDrawFixed
          ItemHeight = 49
          TabOrder = 0
        end
        object btnSetAllSuccess: TButton
          Left = 355
          Top = 39
          Width = 247
          Height = 43
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actSetAllSuccess
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
        end
        object btnCompile: TButton
          Left = 357
          Top = 147
          Width = 245
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actCompile
          TabOrder = 3
        end
        object btnSetAllQuery: TButton
          Left = 355
          Top = 93
          Width = 247
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actSetAllQuery
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
        end
        object pnlViewCompErrs: TPanel
          Left = 355
          Top = 201
          Width = 417
          Height = 126
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          BevelOuter = bvNone
          TabOrder = 5
          object lblViewCompErrsKey: TLabel
            Left = 0
            Top = 47
            Width = 188
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'lblViewCompErrsKey'
          end
          object lblViewCompErrs: TLabel
            Left = 0
            Top = 14
            Width = 155
            Height = 30
            Cursor = crHandPoint
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'lblViewCompErrs'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Segoe UI'
            Font.Style = [fsUnderline]
            ParentFont = False
            OnClick = lblViewCompErrsClick
          end
        end
        object btnViewTestUnit: TButton
          Left = 613
          Top = 147
          Width = 245
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actViewTestUnit
          TabOrder = 4
        end
      end
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
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
    object actViewExtra: TAction
      Caption = 'Previe&w...'
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
    Left = 1178
    Top = 152
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
    Left = 1188
    Top = 50
    object miClearDependencies: TMenuItem
      Action = actClearDependencies
    end
    object miViewDependencies: TMenuItem
      Action = actViewDependencies
    end
  end
  object mnuXRefs: TPopupMenu
    Left = 1186
    Top = 378
    object miClearXRefs: TMenuItem
      Action = actClearXRefs
    end
  end
  object mnuUnits: TPopupMenu
    Left = 1182
    Top = 260
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
