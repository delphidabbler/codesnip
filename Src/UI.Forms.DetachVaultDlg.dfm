inherited DetachVaultDlg: TDetachVaultDlg
  Caption = 'Detach Vault'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblVaults: TLabel
      Left = 0
      Top = 3
      Width = 143
      Height = 13
      Caption = 'Choose &vault to be detached:'
      FocusControl = cbVaults
    end
    object cbVaults: TComboBox
      Left = 0
      Top = 22
      Width = 296
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
