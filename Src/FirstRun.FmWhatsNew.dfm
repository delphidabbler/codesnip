inherited WhatsNewDlg: TWhatsNewDlg
  Caption = 'What'#39's New'
  inherited bvlBottom: TBevel
  end
  inherited pnlBody: TPanel
    inline frmHTML: TWhatsNewHTMLFrame
      Align = alClient
      TabOrder = 0
      TabStop = True
      inherited pnlBrowser: TPanel
        Width = 1674
        Height = 1104
        ExplicitWidth = 1674
        ExplicitHeight = 1104
        inherited wbBrowser: TWebBrowser
          Width = 1674
          Height = 1104
          ExplicitWidth = 1674
          ExplicitHeight = 1104
          ControlData = {
            4C000000DD620000334100000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
  inherited btnHelp: TButton
    Left = 1470
    Top = 1239
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    Visible = False
    ExplicitLeft = 1470
    ExplicitTop = 1239
    ExplicitWidth = 229
    ExplicitHeight = 77
  end
  inherited btnClose: TButton
    Left = 1223
    Top = 1239
    Width = 230
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    ExplicitLeft = 1223
    ExplicitTop = 1239
    ExplicitWidth = 230
    ExplicitHeight = 77
  end
end
