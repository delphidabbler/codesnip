inherited DeleteVaultDlg: TDeleteVaultDlg
  Caption = 'Delete All Snippets From A Vault'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblConfirm: TLabel
      Left = 0
      Top = 256
      Width = 82
      Height = 13
      Caption = 'Confirm &deletion:'
      FocusControl = edConfirm
    end
    object lblVaults: TLabel
      Left = 0
      Top = 224
      Width = 67
      Height = 13
      Caption = 'Choose &vault:'
      FocusControl = cbVaults
    end
    object edConfirm: TEdit
      Left = 120
      Top = 248
      Width = 249
      Height = 21
      TabOrder = 2
    end
    inline frmWarning: TFixedHTMLDlgFrame
      Left = 0
      Top = 0
      Width = 369
      Height = 210
      Align = alTop
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 369
      ExplicitHeight = 210
      inherited pnlBrowser: TPanel
        Width = 369
        Height = 210
        ExplicitWidth = 369
        ExplicitHeight = 210
        inherited wbBrowser: TWebBrowser
          Width = 369
          Height = 210
          ExplicitWidth = 369
          ExplicitHeight = 210
          ControlData = {
            4C00000023260000B41500000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
    object cbVaults: TComboBox
      Left = 120
      Top = 221
      Width = 249
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
