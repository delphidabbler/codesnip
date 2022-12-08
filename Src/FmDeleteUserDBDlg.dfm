inherited DeleteUserDBDlg: TDeleteUserDBDlg
  Caption = 'Delete User Database'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object edConfirm: TEdit
      Left = 0
      Top = 378
      Width = 352
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
    end
    inline frmWarning: TFixedHTMLDlgFrame
      Left = 0
      Top = 0
      Width = 957
      Height = 210
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      TabOrder = 1
      TabStop = True
      ExplicitWidth = 957
      ExplicitHeight = 210
      inherited pnlBrowser: TPanel
        Width = 957
        Height = 210
        ExplicitWidth = 957
        ExplicitHeight = 210
        inherited wbBrowser: TWebBrowser
          Width = 957
          Height = 210
          ExplicitWidth = 957
          ExplicitHeight = 210
          ControlData = {
            4C00000085380000670C00000000000000000000000000000000000000000000
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
