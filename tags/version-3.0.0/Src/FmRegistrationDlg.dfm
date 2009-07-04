inherited RegistrationDlg: TRegistrationDlg
  Caption = 'Registration Wizard'
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
          Width = 195
          Height = 13
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
          Width = 208
          Height = 13
          Caption = 'Please click the Next button below to begin.'
        end
      end
      object tsAboutUser: TTabSheet
        Caption = 'tsAboutUser'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object gbRequired: TGroupBox
          Left = 0
          Top = 8
          Width = 369
          Height = 81
          Caption = ' Required information '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object lblName: TLabel
            Left = 8
            Top = 16
            Width = 353
            Height = 30
            AutoSize = False
            Caption = 
              'The only information about you that is required to complete regs' +
              'tration is your &name. You can use a nickname if your prefer. Pl' +
              'ease enter it below:'
            FocusControl = edName
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
        end
        object gpMailList: TGroupBox
          Left = 0
          Top = 96
          Width = 369
          Height = 137
          Caption = ' CodeSnip mailing list '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          object lblMailListInto: TLabel
            Left = 8
            Top = 16
            Width = 353
            Height = 28
            AutoSize = False
            Caption = 
              'If you would like to join the CodeSnip mailing list please check' +
              ' the box and supply your email address. Learn more about the mai' +
              'ling list.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object lblEmail: TLabel
            Left = 24
            Top = 78
            Width = 94
            Height = 13
            Caption = 'My &email address is:'
            FocusControl = edEmail
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblMailListHelp: TLabel
            Left = 134
            Top = 29
            Width = 154
            Height = 13
            Cursor = crHandPoint
            Caption = 'Learn more about the mailing list.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsUnderline]
            ParentFont = False
            Transparent = False
            OnClick = lblMailListHelpClick
          end
          object lblPrivacy: TLabel
            Left = 8
            Top = 102
            Width = 353
            Height = 26
            AutoSize = False
            Caption = 
              'Your email address will not be abused. See the privacy statement' +
              ' for details.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object lblPrivacyHelp: TLabel
            Left = 231
            Top = 102
            Width = 83
            Height = 13
            Cursor = crHandPoint
            Caption = 'privacy statement'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsUnderline]
            ParentFont = False
            Transparent = False
            OnClick = lblPrivacyHelpClick
          end
          object chkMailList: TCheckBox
            Left = 8
            Top = 52
            Width = 249
            Height = 17
            Caption = 'I would like to join the CodeSnip &mailing list'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            TabOrder = 1
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
          Caption = 
            'You are now ready to submit the registration. Here is the inform' +
            'ation that will be sent:'
          WordWrap = True
        end
        object lblSubmit: TLabel
          Left = 0
          Top = 200
          Width = 369
          Height = 27
          AutoSize = False
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
          Top = 81
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
          Left = 128
          Top = 36
          Width = 233
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
