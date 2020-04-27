inherited DBUpdateDlg: TDBUpdateDlg
  Left = 0
  Top = 0
  Caption = 'Install or Update DelphiDabbler Snippets Database'
  Position = poDesigned
  ExplicitWidth = 565
  ExplicitHeight = 436
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 409
    ExplicitWidth = 409
    inherited pnlHead: TPanel
      Width = 409
      ExplicitWidth = 409
    end
    inherited pcWizard: TPageControl
      Width = 409
      ActivePage = tsFinish
      ExplicitWidth = 409
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        inline frmIntro: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 401
          Height = 236
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 401
          inherited pnlBrowser: TPanel
            Width = 401
            ExplicitWidth = 401
            inherited wbBrowser: TWebBrowser
              Width = 401
              ExplicitWidth = 320
              ExplicitHeight = 240
              ControlData = {
                4C00000072290000641800000000000000000000000000000000000000000000
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
        ImageIndex = 1
        TabVisible = False
        object lblFolder: TLabel
          Left = 0
          Top = 8
          Width = 322
          Height = 13
          Caption = 
            'Enter database download &folder: (click button to browse for fol' +
            'der)'
          FocusControl = edPath
        end
        object lblFolderPageInfo: TLabel
          Left = 0
          Top = 72
          Width = 246
          Height = 13
          Caption = 'Click the Next button to update the local database.'
        end
        object edPath: TEdit
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
      object tsLoad: TTabSheet
        Caption = 'tsLoad'
        ImageIndex = 2
        TabVisible = False
        inline frmLoad: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 316
          Height = 236
          TabOrder = 0
          TabStop = True
          inherited pnlBrowser: TPanel
            inherited wbBrowser: TWebBrowser
              ControlData = {
                4C000000A9200000641800000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 3
        TabVisible = False
        inline frmFinish: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 316
          Height = 236
          TabOrder = 0
          TabStop = True
          inherited pnlBrowser: TPanel
            inherited wbBrowser: TWebBrowser
              ControlData = {
                4C000000A9200000641800000000000000000000000000000000000000000000
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
      ParentBackground = False
      TabOrder = 2
      Visible = False
      ExplicitLeft = 57
      ExplicitHeight = 82
      inherited pnlBody: TPanel
        Height = 82
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
