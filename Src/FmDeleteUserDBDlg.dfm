inherited DeleteUserDBDlg: TDeleteUserDBDlg
  Caption = 'Delete User Database'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object edConfirm: TEdit
      Left = 0
      Top = 216
      Width = 201
      Height = 21
      TabOrder = 0
    end
    inline frmWarning: TFixedHTMLDlgFrame
      Left = 0
      Top = 0
      Width = 369
      Height = 210
      Align = alTop
      TabOrder = 1
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
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
