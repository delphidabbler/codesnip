inherited SWAGImportDlg: TSWAGImportDlg
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'SWAG Import Wizard'
  ClientHeight = 505
  ClientWidth = 687
  ExplicitWidth = 693
  ExplicitHeight = 534
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 671
    Height = 489
    ExplicitWidth = 671
    ExplicitHeight = 489
    inherited pnlHead: TPanel
      Width = 671
      ExplicitWidth = 671
    end
    inherited pcWizard: TPageControl
      Width = 671
      Height = 456
      ActivePage = tsIntro
      ExplicitWidth = 671
      ExplicitHeight = 456
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inline frmIntro: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 663
          Height = 446
          Align = alTop
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 663
          ExplicitHeight = 446
          inherited pnlBrowser: TPanel
            Width = 663
            Height = 446
            ExplicitWidth = 663
            ExplicitHeight = 446
            inherited wbBrowser: TWebBrowser
              Width = 663
              Height = 446
              ExplicitWidth = 663
              ExplicitHeight = 446
              ControlData = {
                4C00000086440000182E00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsFolder: TTabSheet
        Caption = 'tsFolder'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 27
        ExplicitWidth = 0
        ExplicitHeight = 425
        object lblFolder: TLabel
          Left = 0
          Top = 8
          Width = 393
          Height = 13
          Caption = 
            'Enter the SWAG database download &folder: (click the button to b' +
            'rowse for folder)'
          FocusControl = edPath
        end
        object lblFolderPageInfo: TLabel
          Left = 0
          Top = 72
          Width = 270
          Height = 13
          Caption = 'Click the Next button to choose which snippets to import'
        end
        object edPath: TEdit
          Left = 0
          Top = 28
          Width = 484
          Height = 21
          TabOrder = 0
        end
        object btnBrowse: TButton
          Left = 490
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
      object tsCategories: TTabSheet
        Caption = 'tsCategories'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblCategories: TLabel
          Left = 0
          Top = 45
          Width = 87
          Height = 13
          Caption = 'SWAG &categories:'
          FocusControl = lbCategories
        end
        object lblCategoriesDesc: TLabel
          Left = 0
          Top = 3
          Width = 649
          Height = 36
          AutoSize = False
          Caption = 
            'Select a category from the list on the left and click "Show Snip' +
            'pets In Category" (or double click the category) to display a li' +
            'st of its snippets in the right hand list. Tick the snippets you' +
            ' want to import. Repeat with as many categories as you wish. Whe' +
            'n you are ready to import click "Next".'
          WordWrap = True
        end
        object lblSelectSnippets: TLabel
          Left = 256
          Top = 45
          Width = 119
          Height = 13
          Caption = '&Select required snippets:'
          FocusControl = clbSelectSnippets
        end
        object lbCategories: TListBox
          Left = 0
          Top = 64
          Width = 234
          Height = 321
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = lbCategoriesDblClick
          OnKeyDown = lbCategoriesKeyDown
        end
        object clbSelectSnippets: TCheckListBox
          Left = 256
          Top = 64
          Width = 404
          Height = 321
          OnClickCheck = clbSelectSnippetsClickCheck
          ItemHeight = 13
          TabOrder = 2
          OnDblClick = clbSelectSnippetsDblClick
          OnKeyDown = clbSelectSnippetsKeyDown
        end
        object btnDisplayCategory: TButton
          Left = 30
          Top = 391
          Width = 185
          Height = 25
          Action = actDisplayCategory
          TabOrder = 1
        end
        object btnDisplaySnippet: TButton
          Left = 280
          Top = 391
          Width = 185
          Height = 25
          Action = actDisplaySnippet
          TabOrder = 3
        end
      end
      object tsUpdate: TTabSheet
        Caption = 'tsUpdate'
        ImageIndex = 2
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblUpdateDesc: TLabel
          Left = 0
          Top = 3
          Width = 649
          Height = 36
          AutoSize = False
          Caption = 
            'You have chosen to import the following SWAG snippets. They will' +
            ' be imported with the given Display Names and Unique IDs. You ca' +
            'n change these if you wish using the Snippets Editor. To make ch' +
            'anges go back to the previous page. When you are ready to import' +
            ' the snippets click "Import". This step can'#39't be undone.'
          WordWrap = True
        end
        object lvImports: TListView
          Left = 0
          Top = 51
          Width = 660
          Height = 317
          Columns = <
            item
              Caption = 'Snippet Title (Display Name)'
              Width = 400
            end
            item
              Caption = 'Snippet ID (Name)'
              Width = 200
            end>
          ColumnClick = False
          GridLines = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 3
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inline frmOutro: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 663
          Height = 446
          Align = alTop
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 663
          ExplicitHeight = 446
          inherited pnlBrowser: TPanel
            Width = 663
            Height = 446
            ExplicitWidth = 663
            ExplicitHeight = 446
            inherited wbBrowser: TWebBrowser
              Width = 663
              Height = 446
              ExplicitWidth = 320
              ExplicitHeight = 240
              ControlData = {
                4C00000086440000182E00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
    end
  end
  object alWizard: TActionList
    Left = 336
    Top = 256
    object actDisplayCategory: TAction
      Caption = 'S&how Snippets In Category'
      OnExecute = actDisplayCategoryExecute
      OnUpdate = actDisplayCategoryUpdate
    end
    object actDisplaySnippet: TAction
      Caption = '&Preview Selected Snippet...'
      OnExecute = actDisplaySnippetExecute
      OnUpdate = actDisplaySnippetUpdate
    end
    object actBrowse: TAction
      Caption = '...'
      OnExecute = actBrowseExecute
    end
  end
end
