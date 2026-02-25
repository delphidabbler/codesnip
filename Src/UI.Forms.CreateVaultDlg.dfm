inherited CreateVaultDlg: TCreateVaultDlg
  Caption = 'Create Vault'
  ClientHeight = 406
  ExplicitWidth = 565
  ExplicitHeight = 435
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 337
    ExplicitHeight = 337
    inherited pcWizard: TPageControl
      Height = 304
      ActivePage = tsFinish
      ExplicitHeight = 304
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        object lblIntroDesc1: TLabel
          Left = 0
          Top = 3
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 'You can use this wizard to create a new, empty vault.'
          WordWrap = True
        end
        object lblIntroDesc4: TLabel
          Left = 0
          Top = 123
          Width = 366
          Height = 23
          AutoSize = False
          Caption = 'When ready, click the Next button below to begin.'
          WordWrap = True
        end
        object lblIntroDesc2: TLabel
          Left = 0
          Top = 35
          Width = 363
          Height = 74
          AutoSize = False
          Caption = 'You will need to provide the following information:'
          WordWrap = True
        end
        object lblIntroDesc3: TLabel
          Left = 0
          Top = 59
          Width = 363
          Height = 74
          AutoSize = False
          Caption = 
            '- a name for the vault'#13#10'- the data format to be used for the vau' +
            'lt'#13#10'- a directory in which the vault will be stored'#13#10'- any metad' +
            'ata to be associated with the vault'
          WordWrap = True
        end
      end
      object tsName: TTabSheet
        Caption = 'tsName'
        ImageIndex = 4
        TabVisible = False
        object lblName: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 'Enter a &name for the vault:'
          WordWrap = True
        end
        object edName: TEdit
          Left = 0
          Top = 35
          Width = 328
          Height = 21
          TabOrder = 0
        end
      end
      object tsPath: TTabSheet
        Caption = 'tsPath'
        ImageIndex = 2
        TabVisible = False
        object lblPath: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 
            'Enter the full path of the directory where you wish to create th' +
            'e vault:'
          FocusControl = edPath
          WordWrap = True
        end
        object lblPathDesc: TLabel
          Left = 3
          Top = 83
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 
            'The chosen directory must either not exist or, if it already exi' +
            'sts, must be empty.'
          WordWrap = True
        end
        object edPath: TEdit
          Left = 0
          Top = 40
          Width = 332
          Height = 21
          TabOrder = 0
        end
        object btnPath: TButton
          Left = 338
          Top = 40
          Width = 27
          Height = 21
          Action = actPath
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object tsDataFormat: TTabSheet
        Caption = 'tsDataFormat'
        ImageIndex = 5
        TabVisible = False
        object lblDataFormat: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 'Choose a data format for the new vault:'
          WordWrap = True
        end
        object lblDataFormatDesc: TLabel
          Left = 0
          Top = 55
          Width = 334
          Height = 50
          AutoSize = False
          Caption = 
            'Different data formats can store different types of snippet info' +
            'rmation. If in doubt accept the pre-selected format, which is go' +
            'od for most purposes.'
          WordWrap = True
        end
        object cbDataFormat: TComboBox
          Left = 0
          Top = 124
          Width = 321
          Height = 21
          Style = csDropDownList
          TabOrder = 0
        end
      end
      object tsMetaData: TTabSheet
        Caption = 'tsMetaData'
        ImageIndex = 6
        TabVisible = False
        object lblMetaData: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 
            'Enter meta data for the new vault. All fields are optional and m' +
            'ay be left blank.'
          WordWrap = True
        end
        object lblNoMetaData: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 19
          AutoSize = False
          Caption = 'The selected data format does not support meta data.'
          WordWrap = True
        end
        object gbCopyright: TGroupBox
          Left = -3
          Top = 123
          Width = 369
          Height = 112
          Caption = 'Copyright'
          TabOrder = 1
          TabStop = True
          object edCopyrightHolder: TEdit
            Left = 7
            Top = 24
            Width = 246
            Height = 21
            TabOrder = 0
            TextHint = 'Copyright holder'
          end
          object edCopyrightURL: TEdit
            Left = 7
            Top = 51
            Width = 291
            Height = 21
            TabOrder = 2
            TextHint = 'Copyright holder URL'
          end
          object btnCopyrightURL: TButton
            Left = 304
            Top = 49
            Width = 57
            Height = 25
            Action = actCopyrightURL
            TabOrder = 3
          end
          object edCopyrightDate: TEdit
            Left = 259
            Top = 24
            Width = 102
            Height = 21
            TabOrder = 1
            TextHint = 'Date'
          end
          object edContributors: TEdit
            Left = 7
            Top = 80
            Width = 291
            Height = 21
            TabOrder = 4
            TextHint = 'Contributors (comma separated list)'
          end
          object btnEditContributors: TButton
            Left = 328
            Top = 78
            Width = 33
            Height = 25
            Action = actEditContributors
            TabOrder = 5
          end
        end
        object gbLicense: TGroupBox
          Left = -3
          Top = 28
          Width = 369
          Height = 89
          Caption = 'License'
          TabOrder = 0
          TabStop = True
          object cbLicenseSPDX: TComboBox
            Left = 8
            Top = 16
            Width = 97
            Height = 21
            TabOrder = 0
            TextHint = 'SPDX'
            OnChange = cbLicenseSPDXChange
          end
          object edLicenseName: TEdit
            Left = 120
            Top = 16
            Width = 246
            Height = 21
            TabOrder = 1
            TextHint = 'License Name'
          end
          object edLicenseURL: TEdit
            Left = 8
            Top = 51
            Width = 291
            Height = 21
            TabOrder = 2
            TextHint = 'License URL'
          end
          object btnLicenseURL: TButton
            Left = 305
            Top = 51
            Width = 57
            Height = 25
            Action = actLicenseURL
            TabOrder = 3
          end
        end
        object gbAcknowledgements: TGroupBox
          Left = -3
          Top = 238
          Width = 369
          Height = 69
          Caption = 'Acknowledgements'
          TabOrder = 2
          TabStop = True
          object edAcknowledgements: TEdit
            Left = 7
            Top = 27
            Width = 291
            Height = 21
            TabOrder = 0
            TextHint = 'Contributors (comma separated list)'
          end
          object btnEditAcknowledgements: TButton
            Left = 328
            Top = 25
            Width = 33
            Height = 25
            Action = actEditAcknowledgements
            TabOrder = 1
          end
        end
      end
      object tsConfirm: TTabSheet
        Caption = 'tsConfirm'
        ImageIndex = 7
        TabVisible = False
        object lblConfirmDesc1: TLabel
          Left = 0
          Top = 0
          Width = 334
          Height = 50
          AutoSize = False
          Caption = 
            'Ready to create the vault. Please review the information below. ' +
            'If anything is wrong go back to correct it.'
          WordWrap = True
        end
        object lblConfirmDesc2: TLabel
          Left = 0
          Top = 217
          Width = 327
          Height = 13
          AutoSize = False
          Caption = 
            'When content press the Install button. This step cannot be undon' +
            'e.'
          WordWrap = True
        end
        object lvConfirm: TListView
          Left = 3
          Top = 32
          Width = 365
          Height = 179
          Columns = <
            item
              Width = 120
            end
            item
              Width = 200
            end>
          ReadOnly = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 8
        TabVisible = False
        object lblFinishMsg1: TLabel
          Left = 0
          Top = 3
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 'The new vault has been created.'
          WordWrap = True
        end
        object lblFinishMsg2: TLabel
          Left = 3
          Top = 95
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 'Please click the Finish button to close the wizard.'
          WordWrap = True
        end
      end
    end
  end
  inherited btnHelp: TButton
    Left = 300
    Top = 360
    ExplicitLeft = 300
    ExplicitTop = 360
  end
  inherited btnBack: TButton
    Left = 60
    Top = 360
    ExplicitLeft = 60
    ExplicitTop = 360
  end
  inherited btnNext: TButton
    Left = 140
    Top = 360
    ExplicitLeft = 140
    ExplicitTop = 360
  end
  inherited btnCancel: TButton
    Left = 220
    Top = 360
    ExplicitLeft = 220
    ExplicitTop = 360
  end
  object alMain: TActionList
    Left = 16
    Top = 328
    object actLicenseURL: TBrowseURL
      Category = 'Internet'
      Caption = 'View URL'
      OnUpdate = actLicenseURLUpdate
      BeforeBrowse = actLicenseURLBeforeBrowse
    end
    object actCopyrightURL: TBrowseURL
      Category = 'Internet'
      Caption = 'View URL'
      OnUpdate = actCopyrightURLUpdate
      BeforeBrowse = actCopyrightURLBeforeBrowse
    end
    object actPath: TAction
      Caption = '...'
      OnExecute = actPathExecute
    end
    object actEditContributors: TAction
      Caption = '...'
      Hint = 'Edit contributors'
      OnExecute = actEditContributorsExecute
    end
    object actEditAcknowledgements: TAction
      Caption = '...'
      Hint = 'Edit acknowledgements'
      OnExecute = actEditAcknowledgementsExecute
    end
  end
end
