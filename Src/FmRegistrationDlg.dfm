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
          Height = 40
          AutoSize = False
          Caption = 
            'This wizard collects some information from you and the operating' +
            ' system then contacts the DelphiDabbler.com website to register ' +
            'the program.'
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
        object lblName: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 41
          AutoSize = False
          Caption = 
            'The only information about you that is required to complete regs' +
            'tration is your &name. You can use a nickname if your prefer. Pl' +
            'ease enter it below (maximum 48 characters):'
          FocusControl = edName
          WordWrap = True
        end
        object edName: TEdit
          Left = 0
          Top = 55
          Width = 369
          Height = 21
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          MaxLength = 48
          TabOrder = 0
        end
      end
      object tsSubmit: TTabSheet
        Caption = 'tsSubmit'
        ImageIndex = 3
        TabVisible = False
        object lblReport: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 26
          AutoSize = False
          Caption = 
            'You are now ready to submit the registration. Here is the inform' +
            'ation that will be sent:'
          WordWrap = True
        end
        object lblSubmit: TLabel
          Left = 0
          Top = 162
          Width = 369
          Height = 26
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
          Height = 113
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
        object lblThanks: TLabel
          Left = 0
          Top = 8
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'Your registration has been completed successfully - thankyou.'
          WordWrap = True
        end
        object lblRegCode: TLabel
          Left = 0
          Top = 40
          Width = 119
          Height = 13
          Caption = 'Your registration code is: '
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
