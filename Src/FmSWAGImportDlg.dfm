inherited SWAGImportDlg: TSWAGImportDlg
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'SWAG Import Wizard'
  ClientWidth = 1178
  ExplicitWidth = 1190
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Width = 1138
    Height = 640
    ExplicitWidth = 1138
    ExplicitHeight = 640
    inherited pnlHead: TPanel
      Width = 1138
      ExplicitWidth = 1138
    end
    inherited pcWizard: TPageControl
      Width = 1138
      Height = 569
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      ActivePage = tsIntro
      ExplicitTop = 71
      ExplicitWidth = 1138
      ExplicitHeight = 569
      object tsIntro: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsIntro'
        TabVisible = False
        inline frmIntro: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 1130
          Height = 236
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 1130
          ExplicitHeight = 236
          inherited pnlBrowser: TPanel
            Width = 1130
            Height = 236
            ExplicitWidth = 1130
            ExplicitHeight = 236
            inherited wbBrowser: TWebBrowser
              Width = 1130
              Height = 236
              ExplicitWidth = 1130
              ExplicitHeight = 236
              ControlData = {
                4C000000BD420000F00D00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsFolder: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsFolder'
        ImageIndex = 4
        TabVisible = False
        object lblFolder: TLabel
          Left = 0
          Top = 14
          Width = 733
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 
            'Enter the SWAG database download &folder - you need the '#39'swag'#39' s' +
            'ub-directory:'
          FocusControl = edPath
        end
        object lblFolderPageInfo2: TLabel
          Left = 0
          Top = 126
          Width = 522
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Click the Next button to choose which packets to import'
        end
        object lblFolderPageInfo1: TLabel
          Left = 0
          Top = 46
          Width = 393
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Click the ... button to browse for the folder'
          FocusControl = edPath
        end
        object edPath: TEdit
          Left = 5
          Top = 79
          Width = 847
          Height = 38
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 0
        end
        object btnBrowse: TButton
          Left = 863
          Top = 79
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
      object tsCategories: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsCategories'
        ImageIndex = 1
        TabVisible = False
        object lblCategories: TLabel
          Left = 0
          Top = 79
          Width = 164
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'SWAG &categories:'
          FocusControl = lbCategories
        end
        object lblCategoriesDesc: TLabel
          Left = 0
          Top = 5
          Width = 1136
          Height = 63
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'Select a category from the list on the left and click "Show Pack' +
            'ets In Category" (or double click the category) to display a lis' +
            't of its packets in the right hand list. Tick the packet(s) you ' +
            'want to import. Repeat with as many categories as you wish. When' +
            ' you are ready to import click "Next".'
          WordWrap = True
        end
        object lblSelectPackets: TLabel
          Left = 448
          Top = 79
          Width = 219
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '&Select required packets:'
          FocusControl = clbSelectPackets
        end
        object lblVersionNumber: TLabel
          Left = 4
          Top = 747
          Width = 6
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
        end
        object lbCategories: TListBox
          Left = 0
          Top = 112
          Width = 410
          Height = 562
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          ItemHeight = 30
          TabOrder = 0
          OnDblClick = lbCategoriesDblClick
          OnKeyDown = lbCategoriesKeyDown
        end
        object clbSelectPackets: TCheckListBox
          Left = 420
          Top = 112
          Width = 707
          Height = 562
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          ItemHeight = 30
          TabOrder = 2
          OnClickCheck = clbSelectPacketsClickCheck
          OnDblClick = clbSelectPacketsDblClick
          OnKeyDown = clbSelectPacketsKeyDown
        end
        object btnDisplayCategory: TButton
          Left = 53
          Top = 684
          Width = 323
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actDisplayCategory
          TabOrder = 1
        end
        object btnDisplayPacket: TButton
          Left = 490
          Top = 684
          Width = 324
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Action = actDisplayPacket
          TabOrder = 3
        end
      end
      object tsUpdate: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsUpdate'
        ImageIndex = 2
        TabVisible = False
        object lblUpdateDesc: TLabel
          Left = 0
          Top = 5
          Width = 949
          Height = 63
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          AutoSize = False
          Caption = 
            'You have chosen to import the following SWAG packets as CodeSnip' +
            ' snippets. They will be imported with the given Display Names an' +
            'd Packet IDs. You can change these if you wish using the Snippet' +
            's Editor. To make changes go back to the previous page. When you' +
            ' are ready to import the packets click "Import". This step can'#39't' +
            ' be undone.'
          WordWrap = True
        end
        object lvImports: TListView
          Left = 0
          Top = 89
          Width = 1155
          Height = 555
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Columns = <
            item
              Caption = 'Packet Title '#8594' Snippet Display Name'
              Width = 700
            end
            item
              Caption = 'Snippet Name from SWAG ID'
              Width = 350
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
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsFinish'
        ImageIndex = 3
        TabVisible = False
        inline frmOutro: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 1130
          Height = 446
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          DoubleBuffered = False
          ParentDoubleBuffered = False
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 949
          ExplicitHeight = 446
          inherited pnlBrowser: TPanel
            Width = 1130
            Height = 446
            ExplicitWidth = 949
            ExplicitHeight = 446
            inherited wbBrowser: TWebBrowser
              Width = 1130
              Height = 446
              ExplicitWidth = 949
              ExplicitHeight = 446
              ControlData = {
                4C000000BD420000571A00000000000000000000000000000000000000000000
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
      Caption = 'S&how Packets In Category'
      OnExecute = actDisplayCategoryExecute
      OnUpdate = actDisplayCategoryUpdate
    end
    object actDisplayPacket: TAction
      Caption = '&Preview Selected Packet...'
      OnExecute = actDisplayPacketExecute
      OnUpdate = actDisplayPacketUpdate
    end
    object actBrowse: TAction
      Caption = '...'
      OnExecute = actBrowseExecute
    end
  end
end
