inherited CodeSubmitDlg: TCodeSubmitDlg
  Caption = 'Code Submission Wizard'
  ExplicitWidth = 565
  ExplicitHeight = 435
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsFinished
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        inline frmIntro: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 369
          Height = 238
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 369
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 238
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 238
              ControlData = {
                4C00000023260000991800000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsSnippets: TTabSheet
        Caption = 'tsSnippets'
        ImageIndex = 1
        TabVisible = False
        object lblSnippets: TLabel
          Left = 0
          Top = 8
          Width = 198
          Height = 13
          Caption = '&Select the snippet(s) you want to submit:'
          FocusControl = frmSnippets
        end
        object lblSnippetPrompt: TLabel
          Left = 0
          Top = 221
          Width = 221
          Height = 13
          Caption = 'One or more snippets must be selected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
        end
        inline frmSnippets: TSelectSnippetsFrame
          Left = 0
          Top = 32
          Width = 369
          Height = 180
          TabOrder = 0
          TabStop = True
          ExplicitTop = 32
          ExplicitWidth = 369
          ExplicitHeight = 180
          inherited tvChecked: TTreeView
            Width = 369
            Height = 180
            ExplicitWidth = 369
            ExplicitHeight = 180
          end
        end
      end
      object tsUserInfo: TTabSheet
        Caption = 'tsUserInfo'
        ImageIndex = 2
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblName: TLabel
          Left = 0
          Top = 8
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = edName
        end
        object lblEmail: TLabel
          Left = 0
          Top = 43
          Width = 70
          Height = 13
          Caption = '&Email Address:'
          FocusControl = edEMail
        end
        object lblComments: TLabel
          Left = 0
          Top = 104
          Width = 79
          Height = 13
          Caption = 'Your &Comments:'
          FocusControl = edComments
        end
        object edName: TEdit
          Left = 88
          Top = 3
          Width = 185
          Height = 21
          TabOrder = 0
        end
        object edEMail: TEdit
          Left = 88
          Top = 40
          Width = 185
          Height = 21
          TabOrder = 1
        end
        object edComments: TMemo
          Left = 0
          Top = 123
          Width = 369
          Height = 112
          TabOrder = 3
        end
        inline frmPrivacy: TFixedHTMLDlgFrame
          Left = 0
          Top = 69
          Width = 369
          Height = 5
          TabOrder = 2
          TabStop = True
          ExplicitTop = 69
          ExplicitWidth = 369
          ExplicitHeight = 5
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 5
            ExplicitWidth = 369
            ExplicitHeight = 18
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 5
              TabStop = False
              ExplicitTop = -2
              ExplicitWidth = 369
              ExplicitHeight = 18
              ControlData = {
                4C00000023260000840000000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsLicense: TTabSheet
        Caption = 'tsLicense'
        ImageIndex = 5
        TabVisible = False
        ExplicitLeft = 8
        ExplicitTop = 11
        object chkAgreeLicense: TCheckBox
          Left = 3
          Top = 200
          Width = 363
          Height = 17
          Caption = 'I confirm my &agreement with statements 1 and 2 above'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        inline frmLicenseTerms: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 369
          Height = 169
          TabOrder = 1
          TabStop = True
          ExplicitWidth = 369
          ExplicitHeight = 169
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 169
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 169
              ControlData = {
                4C00000023260000771100000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsSubmit: TTabSheet
        Caption = 'tsSubmit'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        object btnPreview: TButton
          Left = 136
          Top = 164
          Width = 98
          Height = 25
          Caption = '&Preview Data...'
          TabOrder = 0
          OnClick = btnPreviewClick
        end
        inline frmSubmit: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 366
          Height = 145
          TabOrder = 1
          TabStop = True
          ExplicitWidth = 366
          ExplicitHeight = 145
          inherited pnlBrowser: TPanel
            Width = 366
            Height = 145
            inherited wbBrowser: TWebBrowser
              Width = 366
              Height = 145
              ControlData = {
                4C000000D4250000FC0E00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsFinished: TTabSheet
        Caption = 'tsFinished'
        ImageIndex = 3
        TabVisible = False
        inline frmFinished: TFixedHTMLDlgFrame
          Left = 0
          Top = 0
          Width = 369
          Height = 238
          Align = alClient
          TabOrder = 0
          TabStop = True
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 238
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 238
              ControlData = {
                4C00000023260000991800000000000000000000000000000000000000000000
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
