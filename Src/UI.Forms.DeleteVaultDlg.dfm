inherited DeleteVaultDlg: TDeleteVaultDlg
  Caption = 'Clear Or Delete Vault'
  ClientHeight = 401
  ExplicitWidth = 474
  ExplicitHeight = 430
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 401
    ExplicitWidth = 401
    object lblConfirm: TLabel
      Left = 0
      Top = 232
      Width = 412
      Height = 13
      Caption = 
        'Confir&m deletion by entering the name of the chosen vault below' +
        ' (not case sensitive):'
      FocusControl = edConfirm
      WordWrap = True
    end
    object lblVaults: TLabel
      Left = 0
      Top = 194
      Width = 67
      Height = 13
      Caption = 'Choose &vault:'
      FocusControl = cbVaults
    end
    object lblClearOrDelete: TLabel
      Left = 0
      Top = 138
      Width = 360
      Height = 13
      Caption = 
        'After deleting the snippets do you want to keep or delete the em' +
        'pty vault?'
    end
    object edConfirm: TEdit
      Left = 120
      Top = 261
      Width = 249
      Height = 21
      TabOrder = 4
    end
    inline frmWarning: TFixedHTMLDlgFrame
      Left = 0
      Top = 0
      Width = 401
      Height = 121
      Align = alTop
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 401
      ExplicitHeight = 121
      inherited pnlBrowser: TPanel
        Width = 401
        Height = 121
        ExplicitWidth = 401
        ExplicitHeight = 121
        inherited wbBrowser: TWebBrowser
          Width = 401
          Height = 121
          ExplicitTop = 5
          ExplicitWidth = 369
          ExplicitHeight = 210
          ControlData = {
            4C00000072290000810C00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
    object cbVaults: TComboBox
      Left = 73
      Top = 191
      Width = 296
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object rbDeleteVault: TRadioButton
      Left = 143
      Top = 163
      Width = 113
      Height = 17
      Caption = '&Delete the vault'
      TabOrder = 2
    end
    object rbKeepVault: TRadioButton
      Left = 24
      Top = 163
      Width = 113
      Height = 17
      Caption = '&Keep the vault'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
