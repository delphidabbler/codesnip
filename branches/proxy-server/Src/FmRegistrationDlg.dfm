inherited RegistrationDlg: TRegistrationDlg
  Caption = 'Registration Wizard'
  ExplicitWidth = 565
  ExplicitHeight = 433
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsAboutUser
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblIntro: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'Thanks for deciding to register CodeSnip.'
        end
        object lblIntroExplain: TLabel
          Left = 0
          Top = 33
          Width = 369
          Height = 32
          AutoSize = False
          Caption = 
            'This wizard collects some information from you then contacts the' +
            ' DelphiDabbler.com website to register the program.'
          WordWrap = True
        end
        object lblInstructions: TLabel
          Left = 0
          Top = 72
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'Please click the Next button below to begin.'
        end
      end
      object tsAboutUser: TTabSheet
        Caption = 'tsAboutUser'
        ImageIndex = 1
        TabVisible = False
        object gbRequired: TGroupBox
          Left = 0
          Top = 8
          Width = 369
          Height = 81
          Caption = ' Required information '
          TabOrder = 0
          object lblName: TLabel
            Left = 8
            Top = 20
            Width = 353
            Height = 30
            AutoSize = False
            Caption = 
              'The only information about you that is required to complete regs' +
              'tration is your &name. You can use a nickname if your prefer. Pl' +
              'ease enter it below:'
            FocusControl = edName
            WordWrap = True
          end
          object edName: TEdit
            Left = 8
            Top = 48
            Width = 353
            Height = 21
            BevelInner = bvSpace
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            TabOrder = 0
          end
        end
        object gbMailList: TGroupBox
          Left = 0
          Top = 95
          Width = 369
          Height = 137
          Caption = ' CodeSnip mailing list '
          TabOrder = 1
          object lblEmail: TLabel
            Left = 24
            Top = 78
            Width = 68
            Height = 13
            Caption = '&Email address:'
            FocusControl = edEmail
          end
          object chkMailList: TCheckBox
            Left = 8
            Top = 55
            Width = 353
            Height = 17
            Caption = 'I would like to join the CodeSnip &mailing list'
            TabOrder = 1
            OnClick = chkMailListClick
          end
          object edEmail: TEdit
            Left = 128
            Top = 74
            Width = 233
            Height = 21
            BevelInner = bvSpace
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            Enabled = False
            ParentColor = True
            TabOrder = 2
          end
          inline frmMailListIntro: TFixedHTMLDlgFrame
            Left = 8
            Top = 24
            Width = 353
            Height = 26
            TabOrder = 0
            TabStop = True
            ExplicitLeft = 8
            ExplicitTop = 24
            ExplicitWidth = 353
            ExplicitHeight = 26
            inherited pnlBrowser: TPanel
              Width = 353
              Height = 26
              ExplicitWidth = 353
              ExplicitHeight = 26
              inherited wbBrowser: TWebBrowser
                Width = 353
                Height = 26
                TabStop = False
                ExplicitTop = -1
                ExplicitWidth = 353
                ExplicitHeight = 26
                ControlData = {
                  4C0000007C240000B00200000000000000000000000000000000000000000000
                  000000004C000000000000000000000001000000E0D057007335CF11AE690800
                  2B2E126208000000000000004C0000000114020000000000C000000000000046
                  8000000000000000000000000000000000000000000000000000000000000000
                  00000000000000000100000000000000000000000000000000000000}
              end
            end
          end
          inline frmPrivacy: TFixedHTMLDlgFrame
            Left = 8
            Top = 102
            Width = 353
            Height = 26
            TabOrder = 3
            TabStop = True
            ExplicitLeft = 8
            ExplicitTop = 102
            ExplicitWidth = 353
            ExplicitHeight = 26
            inherited pnlBrowser: TPanel
              Width = 353
              Height = 26
              ExplicitWidth = 353
              ExplicitHeight = 26
              inherited wbBrowser: TWebBrowser
                Width = 353
                Height = 26
                TabStop = False
                ExplicitWidth = 353
                ExplicitHeight = 26
                ControlData = {
                  4C0000007C240000B00200000000000000000000000000000000000000000000
                  000000004C000000000000000000000001000000E0D057007335CF11AE690800
                  2B2E126208000000000000004C0000000114020000000000C000000000000046
                  8000000000000000000000000000000000000000000000000000000000000000
                  00000000000000000100000000000000000000000000000000000000}
              end
            end
          end
        end
      end
      object tsSubmit: TTabSheet
        Caption = 'tsSubmit'
        ImageIndex = 3
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblReport: TLabel
          Left = 0
          Top = 8
          Width = 358
          Height = 26
          AutoSize = False
          Caption = 
            'You are now ready to submit the registration. Here is the inform' +
            'ation that will be sent:'
          WordWrap = True
        end
        object lblSubmit: TLabel
          Left = 0
          Top = 200
          Width = 353
          Height = 26
          Caption = 
            'Please ensure you are connected to the internet and then click t' +
            'he Submit button to send the registration.'
          WordWrap = True
        end
        object edReport: TMemo
          Left = 0
          Top = 40
          Width = 369
          Height = 153
          TabStop = False
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblThanks: TLabel
          Left = 0
          Top = 8
          Width = 291
          Height = 13
          Caption = 'Your registration has been completed successfully - thankyou.'
        end
        object lblRegCode: TLabel
          Left = 0
          Top = 40
          Width = 119
          Height = 13
          Caption = 'Your registration code is: '
        end
        object lblMailListConfirm: TLabel
          Left = 0
          Top = 105
          Width = 369
          Height = 48
          AutoSize = False
          Caption = 
            'You chose to subscribe the the CodeSnip mailing list. For your o' +
            'wn protection you need to confirm your subscription. An email ha' +
            's been sent to the address you provided that explains how to do ' +
            'this.'
          WordWrap = True
        end
        object edRegCode: TEdit
          Left = 0
          Top = 59
          Width = 249
          Height = 21
          TabStop = False
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
  end
end
