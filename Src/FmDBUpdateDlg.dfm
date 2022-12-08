inherited DBUpdateDlg: TDBUpdateDlg
  Caption = 'Install or Update DelphiDabbler Snippets Database'
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      ActivePage = tsIntro
      ExplicitTop = 71
      ExplicitHeight = 560
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
          Width = 401
          Height = 236
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 401
          ExplicitHeight = 236
          inherited pnlBrowser: TPanel
            Width = 401
            Height = 236
            ExplicitWidth = 401
            ExplicitHeight = 236
            inherited wbBrowser: TWebBrowser
              Width = 401
              Height = 236
              ExplicitWidth = 401
              ExplicitHeight = 236
              ControlData = {
                4C000000AF170000F00D00000000000000000000000000000000000000000000
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
        ImageIndex = 1
        TabVisible = False
        object lblFolder: TLabel
          Left = 0
          Top = 14
          Width = 615
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 
            'Enter database download &folder: (click button to browse for fol' +
            'der)'
          FocusControl = edPath
        end
        object lblFolderPageInfo: TLabel
          Left = 0
          Top = 126
          Width = 421
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Click the Next button when ready to proceed.'
        end
        object edPath: TEdit
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
      object tsLoad: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsLoad'
        ImageIndex = 2
        TabVisible = False
        inline frmLoad: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 316
          Height = 236
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 316
          ExplicitHeight = 236
          inherited pnlBrowser: TPanel
            Width = 316
            Height = 236
            ExplicitWidth = 316
            ExplicitHeight = 236
            inherited wbBrowser: TWebBrowser
              Width = 316
              Height = 236
              ExplicitWidth = 316
              ExplicitHeight = 236
              ControlData = {
                4C000000AA120000F00D00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
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
        inline frmFinish: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 316
          Height = 236
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 316
          ExplicitHeight = 236
          inherited pnlBrowser: TPanel
            Width = 316
            Height = 236
            ExplicitWidth = 316
            ExplicitHeight = 236
            inherited wbBrowser: TWebBrowser
              Width = 316
              Height = 236
              ExplicitWidth = 316
              ExplicitHeight = 236
              ControlData = {
                4C000000AA120000F00D00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
    end
    inline frmProgress: TProgressFrame
      Left = 57
      Top = 0
      Width = 320
      Height = 82
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ParentBackground = False
      TabOrder = 2
      Visible = False
      ExplicitLeft = 57
      ExplicitWidth = 320
      ExplicitHeight = 82
      inherited pnlBody: TPanel
        Width = 320
        Height = 82
        ExplicitWidth = 320
        ExplicitHeight = 82
      end
    end
  end
  object alMain: TActionList
    Left = 448
    Top = 208
    object actBrowse: TAction
      Caption = '...'
      Hint = 'Browse for database folder'
      OnExecute = actBrowseExecute
    end
  end
end
