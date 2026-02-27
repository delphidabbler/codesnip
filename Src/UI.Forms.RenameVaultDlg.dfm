inherited RenameVaultDlg: TRenameVaultDlg
  Caption = 'Rename Vault'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblVaults: TLabel
      Left = 0
      Top = 3
      Width = 67
      Height = 13
      Caption = 'Choose &vault:'
      FocusControl = cbVaults
    end
    object lblNewName: TLabel
      Left = 0
      Top = 48
      Width = 82
      Height = 13
      Caption = 'Enter new &name:'
      FocusControl = edNewName
    end
    object cbVaults: TComboBox
      Left = 0
      Top = 22
      Width = 296
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object edNewName: TEdit
      Left = 0
      Top = 67
      Width = 299
      Height = 21
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
