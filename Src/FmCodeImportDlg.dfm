inherited CodeImportDlg: TCodeImportDlg
  Caption = 'Import Wizard'
  ClientWidth = 982
  ExplicitWidth = 994
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Width = 953
    ExplicitWidth = 953
    inherited pnlHead: TPanel
      Width = 953
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ExplicitWidth = 953
      ExplicitHeight = 43
      inherited lblHead: TLabel
        Left = 10
        Top = 4
        Margins.Left = 9
        Margins.Top = 9
        Margins.Right = 9
        Margins.Bottom = 9
        ExplicitLeft = 10
        ExplicitTop = 4
      end
    end
    inherited pcWizard: TPageControl
      Top = 43
      Width = 953
      Height = 588
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      ActivePage = tsFinish
      ExplicitTop = 43
      ExplicitWidth = 953
      ExplicitHeight = 588
      object tsInfo: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsInfo'
        TabVisible = False
        object lblIntro: TLabel
          Left = 0
          Top = 14
          Width = 646
          Height = 397
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'This wizard helps you import snippets from a file into your user' +
            ' database.'#13#10#13#10'Click the Next button below to begin.'
          WordWrap = True
        end
      end
      object tsFile: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsFile'
        ImageIndex = 1
        TabVisible = False
        object lblFile: TLabel
          Left = 0
          Top = 14
          Width = 446
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Open import &file: (click button to browse for file)'
          FocusControl = edFile
        end
        object lblLoadFile: TLabel
          Left = 0
          Top = 126
          Width = 344
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Click the Next button to read the file.'
        end
        object edFile: TEdit
          Left = 0
          Top = 49
          Width = 581
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 0
        end
        object btnBrowse: TButton
          Left = 592
          Top = 49
          Width = 47
          Height = 37
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actBrowse
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object tsUpdate: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsUpdate'
        ImageIndex = 3
        TabVisible = False
        object lblImportList: TLabel
          Left = 0
          Top = 121
          Width = 173
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Imported &snippets:'
          FocusControl = lvImports
        end
        object lblSelectedSnippet: TLabel
          Left = 0
          Top = 411
          Width = 157
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'S&elected snippet:'
          FocusControl = edRename
        end
        object lblModifyInstructions: TLabel
          Left = 0
          Top = 14
          Width = 646
          Height = 97
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'The functions to be imported are listed below. Select, deselect ' +
            'and rename as required then click the "Update" button to update ' +
            'the database. Use "Cancel" to abandon the import.'
          WordWrap = True
        end
        object lvImports: TListView
          Left = 0
          Top = 166
          Width = 646
          Height = 243
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Checkboxes = True
          Columns = <
            item
              Caption = 'Snippet Name'
              Width = 245
            end
            item
              Caption = 'Import Using Name'
              Width = 245
            end
            item
              Caption = 'Action'
              Width = 105
            end>
          ColumnClick = False
          GridLines = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lvImportsSelectItem
          OnItemChecked = lvImportsItemChecked
        end
        object btnRename: TButton
          Left = 282
          Top = 444
          Width = 131
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actRename
          TabOrder = 1
        end
        object edRename: TEdit
          Left = 0
          Top = 448
          Width = 271
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 2
        end
      end
      object tsFinish: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsFinish'
        ImageIndex = 5
        TabVisible = False
        object lblFinish: TLabel
          Left = 0
          Top = 14
          Width = 646
          Height = 58
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'The database has now been updated. The following snippets were i' +
            'mported:'
          WordWrap = True
        end
        object sbFinish: TScrollBox
          Left = 33
          Top = 88
          Width = 585
          Height = 371
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          BorderStyle = bsNone
          TabOrder = 0
        end
      end
    end
  end
  object alMain: TActionList
    Left = 728
    Top = 488
    object actRename: TAction
      Caption = '&Rename...'
      OnExecute = actRenameExecute
      OnUpdate = actRenameUpdate
    end
    object actBrowse: TAction
      Caption = '...'
      Hint = 'Browse for import file'
      OnExecute = actBrowseExecute
    end
  end
end
