inherited SWAGImportDlg: TSWAGImportDlg
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'SWAG Import Wizard'
  ClientHeight = 505
  ClientWidth = 687
  ExplicitWidth = 693
  ExplicitHeight = 533
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
      ActivePage = tsUpdate
      ExplicitWidth = 671
      ExplicitHeight = 456
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        inline frmIntro: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 663
          Height = 446
          Align = alClient
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
      object tsCategories: TTabSheet
        Caption = 'tsCategories'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 8
        ExplicitTop = -10
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
            'Double-click a category from the list on the left and a list of ' +
            'its snippets will be displayed in right hand list. Tick the snip' +
            'pets you want to import in the right hand list. Double-click and' +
            ' snippet to view it. Repeat with as many categories as you wish.' +
            ' When you are ready to import click "Next".'
          WordWrap = True
        end
        object lblSelectSnippets: TLabel
          Left = 256
          Top = 45
          Width = 145
          Height = 13
          Caption = '&Snippets in selected category:'
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
          TabOrder = 1
          OnDblClick = clbSelectSnippetsDblClick
          OnKeyDown = clbSelectSnippetsKeyDown
        end
      end
      object tsUpdate: TTabSheet
        Caption = 'tsUpdate'
        ImageIndex = 2
        TabVisible = False
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
        inline frmOutro: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 663
          Height = 446
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 280
          ExplicitTop = 64
          inherited pnlBrowser: TPanel
            Width = 663
            Height = 446
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
end
