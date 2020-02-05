inherited V4ConfigDlg: TV4ConfigDlg
  Left = 0
  Top = 75
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'First Run Wizard'
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 565
  ExplicitHeight = 435
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsUserDB
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblIntro1: TLabel
          Left = 0
          Top = 3
          Width = 369
          Height = 38
          AutoSize = False
          Caption = 
            'This appears to be the first time you have run CodeSnip since up' +
            'dating to version 4.'
          WordWrap = True
        end
        object lblIntro2: TLabel
          Left = 0
          Top = 39
          Width = 369
          Height = 34
          AutoSize = False
          Caption = 'A little configuration is needed before the program starts.'
          WordWrap = True
        end
        object lblIntro4: TLabel
          Left = 0
          Top = 137
          Width = 369
          Height = 30
          AutoSize = False
          Caption = 'For more information click "Help".'
          WordWrap = True
        end
        object lblIntro5: TLabel
          Left = 0
          Top = 165
          Width = 369
          Height = 30
          AutoSize = False
          Caption = 'When you are ready to start click "Next".'
          WordWrap = True
        end
        object lblIntro3: TLabel
          Left = 0
          Top = 71
          Width = 369
          Height = 44
          AutoSize = False
          Caption = 
            'This is a one-time only operation. You should not see this dialo' +
            'gue box again unless you run the program again when logged on as' +
            ' a different user.'
          WordWrap = True
        end
      end
      object tsConfigFile: TTabSheet
        Caption = 'tsConfigFile'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblCopyConfig: TLabel
          Left = 0
          Top = 3
          Width = 369
          Height = 47
          AutoSize = False
          Caption = 
            'You can keep many of your preferences from your previous version' +
            ' of CodeSnip. If you don'#39't want to do this just clear the check ' +
            'box below.'
          WordWrap = True
        end
        object chkCopyConfig: TCheckBox
          Left = 0
          Top = 56
          Width = 369
          Height = 17
          Caption = 'Preserve &preferences from previous version'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
      end
      object tsUserDB: TTabSheet
        Caption = 'tsUserDB'
        ImageIndex = 2
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblUserDB1: TLabel
          Left = 0
          Top = 3
          Width = 369
          Height = 38
          AutoSize = False
          Caption = 
            'A user defined snippets database that was created by an earlier ' +
            'version of CodeSnip has been found. This database can be copied ' +
            'for use by CodeSnip 4. Clear the following check box if you do n' +
            'ot want to do this.'
          WordWrap = True
        end
        object lblUserDB2: TLabel
          Left = 0
          Top = 89
          Width = 369
          Height = 38
          AutoSize = False
          Caption = 
            'Any changes you make to the new copy of the database will not af' +
            'fect the previous version and vice-versa.'
          WordWrap = True
        end
        object chkCopyDB: TCheckBox
          Left = 0
          Top = 66
          Width = 369
          Height = 17
          Caption = 'Copy existing &database'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
      end
      object tsSummary: TTabSheet
        Caption = 'tsSummary'
        ImageIndex = 3
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblSummaryPrefix: TLabel
          Left = 0
          Top = 3
          Width = 99
          Height = 13
          Caption = 'You have chosen to:'
        end
        object lblSummaryPostfix1: TLabel
          Left = 0
          Top = 111
          Width = 367
          Height = 42
          AutoSize = False
          Caption = 
            'If you wish to change anything, use the "Back" button to go back' +
            ' to the relevant page.'
          WordWrap = True
        end
        object lblSummaryPostfix2: TLabel
          Left = -1
          Top = 159
          Width = 367
          Height = 42
          AutoSize = False
          Caption = 
            'When you are content, click "Confirm". Once you do this you can'#39 +
            't change your mind.'
          WordWrap = True
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
        object lblFinish1: TLabel
          Left = 0
          Top = 3
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'CodeSnip 4 is now configured.'
          WordWrap = True
        end
        object lblFinish2: TLabel
          Left = 0
          Top = 22
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'Your old preferences have been kept, with the following changes:'
          WordWrap = True
        end
        object lblFinish3: TLabel
          Left = 0
          Top = 41
          Width = 369
          Height = 13
          AutoSize = False
          Caption = 'Click "Finish" to close this Wizard and start CodeSnip.'
          WordWrap = True
        end
      end
    end
  end
end
