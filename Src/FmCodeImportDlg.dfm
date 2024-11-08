inherited CodeImportDlg: TCodeImportDlg
  Caption = 'Import Wizard'
  ExplicitWidth = 565
  ExplicitHeight = 436
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 321
    ExplicitHeight = 321
    inherited pcWizard: TPageControl
      Height = 288
      ActivePage = tsInfo
      ExplicitHeight = 288
      object tsInfo: TTabSheet
        Caption = 'tsInfo'
        TabVisible = False
        object lblIntro: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 227
          AutoSize = False
          Caption = 
            'This wizard helps you import snippets from a file into your user' +
            ' database.'#13#10#13#10'Click the Next button below to begin.'
          WordWrap = True
        end
      end
      object tsFile: TTabSheet
        Caption = 'tsFile'
        ImageIndex = 1
        TabVisible = False
        object lblFile: TLabel
          Left = 0
          Top = 8
          Width = 230
          Height = 13
          Caption = 'Open import &file: (click button to browse for file)'
          FocusControl = edFile
        end
        object lblLoadFile: TLabel
          Left = 0
          Top = 72
          Width = 179
          Height = 13
          Caption = 'Click the Next button to read the file.'
        end
        object edFile: TEdit
          Left = 0
          Top = 28
          Width = 332
          Height = 21
          TabOrder = 0
        end
        object btnBrowse: TButton
          Left = 338
          Top = 28
          Width = 27
          Height = 21
          Action = actBrowse
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object tsUpdate: TTabSheet
        Caption = 'tsUpdate'
        ImageIndex = 3
        TabVisible = False
        object lblImportList: TLabel
          Left = 0
          Top = 53
          Width = 91
          Height = 13
          Caption = 'Imported &snippets:'
          FocusControl = lvImports
        end
        object lblModifyInstructions: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 40
          AutoSize = False
          Caption = 
            'The functions to be imported are listed below. Clear the check b' +
            'oxes next to any snippets you don'#39't want to import then click th' +
            'e "Update" button to update the database. Use "Cancel" to abando' +
            'n the import.'
          WordWrap = True
        end
        object lvImports: TListView
          Left = 0
          Top = 72
          Width = 369
          Height = 139
          Checkboxes = True
          Columns = <
            item
              Caption = 'Snippet'
              Width = 280
            end
            item
              Caption = 'Action'
              Width = 60
            end>
          ColumnClick = False
          GridLines = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnItemChecked = lvImportsItemChecked
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 5
        TabVisible = False
        object lblFinish: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 33
          AutoSize = False
          Caption = 
            'The database has now been updated. The following snippets were i' +
            'mported:'
          WordWrap = True
        end
        object sbFinish: TScrollBox
          Left = 19
          Top = 31
          Width = 334
          Height = 212
          BorderStyle = bsNone
          TabOrder = 0
        end
      end
    end
  end
  object alMain: TActionList
    Left = 448
    Top = 208
    object actBrowse: TAction
      Caption = '...'
      Hint = 'Browse for import file'
      OnExecute = actBrowseExecute
    end
  end
end
