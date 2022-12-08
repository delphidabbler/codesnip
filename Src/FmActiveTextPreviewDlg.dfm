inherited ActiveTextPreviewDlg: TActiveTextPreviewDlg
  Caption = 'Markup Preview'
  ClientWidth = 967
  ExplicitWidth = 979
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    inline frmPreview: THTMLTpltDlgFrame
      Left = 0
      Top = 0
      Width = 957
      Height = 631
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 957
      ExplicitHeight = 631
      inherited pnlBrowser: TPanel
        Width = 957
        Height = 631
        ExplicitWidth = 957
        ExplicitHeight = 631
        inherited wbBrowser: TWebBrowser
          Width = 957
          Height = 631
          ExplicitWidth = 957
          ExplicitHeight = 631
          ControlData = {
            4C00000085380000442500000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
end
