inherited CompErrorDlg: TCompErrorDlg
  Caption = 'Compiler Errors & Warnings'
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    inline frmHTML: THTMLTpltDlgFrame
      Left = 0
      Top = 0
      Width = 553
      Height = 413
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      TabStop = True
      inherited pnlBrowser: TPanel
        inherited wbBrowser: TWebBrowser
          ControlData = {
            4C000000A9200000641800000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
    object tsCompilers: TTabSet
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 957
      Height = 44
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 11
      Align = alTop
      DitherBackground = False
      EndMargin = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      StartMargin = 4
      SoftTop = True
      Style = tsSoftTabs
      TabPosition = tpTop
      OnChange = tsCompilersChange
    end
  end
  object alTabs: TActionList
    Left = 16
    Top = 48
    object actNextTab: TAction
      Tag = 1
      Caption = 'actNextTab'
      ShortCut = 16393
      OnExecute = TabShortcutExecute
      OnUpdate = TabShortcutUpdate
    end
    object actPrevTab: TAction
      Tag = -1
      Caption = 'actPrevTab'
      ShortCut = 24585
      OnExecute = TabShortcutExecute
      OnUpdate = TabShortcutUpdate
    end
  end
end
