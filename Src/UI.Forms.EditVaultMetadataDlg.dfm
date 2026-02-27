inherited EditVaultMetadataDlg: TEditVaultMetadataDlg
  Caption = 'Edit Vault Metadata'
  ClientHeight = 408
  ExplicitWidth = 474
  ExplicitHeight = 437
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 353
    ExplicitHeight = 353
    object lblVaults: TLabel
      Left = 0
      Top = 3
      Width = 67
      Height = 13
      Caption = 'Choose &vault:'
      FocusControl = cbVaults
    end
    object lblNoMetaData: TLabel
      Left = 6
      Top = 35
      Width = 334
      Height = 38
      AutoSize = False
      Caption = 'The selected vault'#39's data format does not support meta data.'
      WordWrap = True
    end
    object cbVaults: TComboBox
      Left = 73
      Top = 0
      Width = 296
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbVaultsChange
    end
    object gbLicense: TGroupBox
      Left = 0
      Top = 60
      Width = 369
      Height = 89
      Caption = 'License'
      TabOrder = 1
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
        OnChange = edLicenseURLChange
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
    object gbCopyright: TGroupBox
      Left = 0
      Top = 155
      Width = 369
      Height = 112
      Caption = 'Copyright'
      TabOrder = 2
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
        OnChange = edCopyrightURLChange
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
    object gbAcknowledgements: TGroupBox
      Left = 0
      Top = 270
      Width = 369
      Height = 69
      Caption = 'Acknowledgements'
      TabOrder = 3
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
  inherited btnHelp: TButton
    Top = 375
    ExplicitTop = 375
  end
  inherited btnCancel: TButton
    Top = 375
    ExplicitTop = 375
  end
  inherited btnOK: TButton
    Top = 375
    OnClick = btnOKClick
    ExplicitTop = 375
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
