inherited CodeSubmitDlg: TCodeSubmitDlg
  Caption = 'Code Submission Wizard'
  ExplicitWidth = 565
  ExplicitHeight = 433
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsSnippets
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        object lblIntro: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 227
          AutoSize = False
          Caption = 
            'Thanks for deciding to submit code for inclusion in the Code Sni' +
            'ppets database.'#13#10#13#10'This wizard collects some information from yo' +
            'u then contacts the DelphiDabbler.com website to submit the code' +
            '.'#13#10#13#10'Please note that you must have the right to donate this cod' +
            'e and you must be prepared to release the code into the public d' +
            'omain.'#13#10#13#10'Click the Next button below to begin.'
          WordWrap = True
        end
      end
      object tsSnippets: TTabSheet
        Caption = 'tsSnippets'
        ImageIndex = 1
        TabVisible = False
        object lblSnippets: TLabel
          Left = 0
          Top = 8
          Width = 190
          Height = 13
          Caption = '&Select the snippet(s) you want to submit:'
          FocusControl = frmSnippets
        end
        object lblSnippetPrompt: TLabel
          Left = 0
          Top = 221
          Width = 183
          Height = 13
          Caption = 'One or more snippets must be selected'
          Visible = False
        end
        inline frmSnippets: TSelectUserSnippetsFrame
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
          Width = 69
          Height = 13
          Caption = '&Email Address:'
          FocusControl = edEMail
        end
        object lblComments: TLabel
          Left = 0
          Top = 104
          Width = 77
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
          Height = 18
          TabOrder = 2
          TabStop = True
          ExplicitTop = 69
          ExplicitWidth = 369
          ExplicitHeight = 18
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 18
            ExplicitWidth = 369
            ExplicitHeight = 18
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 18
              TabStop = False
              ExplicitTop = -2
              ExplicitWidth = 369
              ExplicitHeight = 18
              ControlData = {
                4C00000023260000DC0100000000000000000000000000000000000000000000
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
        object lblSubmit: TLabel
          Left = 0
          Top = 3
          Width = 369
          Height = 127
          AutoSize = False
          Caption = 
            'Ready to submit the code.'#13#10#13#10'The data that will be sent contains' +
            ' the specified snippet(s), the information your gave on the prev' +
            'ious page and your program'#39's version number.'#13#10#13#10'If you want to s' +
            'ee the data before sending it, click the Preview Data button.'#13#10#13 +
            #10'To proceed please ensure you are connected to the internet and ' +
            'click the Submit button.'
          WordWrap = True
        end
        object btnPreview: TButton
          Left = 136
          Top = 144
          Width = 98
          Height = 25
          Caption = '&Preview Data...'
          TabOrder = 0
          OnClick = btnPreviewClick
        end
      end
      object tsFinished: TTabSheet
        Caption = 'tsFinished'
        ImageIndex = 3
        TabVisible = False
        object lblFinished: TLabel
          Left = 0
          Top = 0
          Width = 369
          Height = 201
          AutoSize = False
          Caption = 
            'Thank you. Your submission has been sent successfully.'#13#10#13#10'The da' +
            'tabase editor will review the code and, if it is in a suitable f' +
            'ormat and it works as advertised, it will be included in the onl' +
            'ine Code Snippets database in due course. You may be contacted i' +
            'f there are any queries.'
          WordWrap = True
        end
      end
    end
  end
end
