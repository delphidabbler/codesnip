inherited ActiveTextPreviewDlg: TActiveTextPreviewDlg
  Caption = 'Markup Preview'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inline frmPreview: THTMLTpltDlgFrame
      Left = 0
      Top = 0
      Width = 377
      Height = 281
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 377
      ExplicitHeight = 281
      inherited pnlBrowser: TPanel
        Width = 377
        Height = 281
        ExplicitWidth = 377
        ExplicitHeight = 281
        inherited wbBrowser: TWebBrowser
          Width = 377
          Height = 281
          ExplicitWidth = 320
          ExplicitHeight = 240
          ControlData = {
            4C000000F72600000B1D00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
  inherited btnHelp: TButton
    Visible = False
  end
end
