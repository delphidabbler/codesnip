inherited CompErrorDlg: TCompErrorDlg
  Caption = 'CompErrorDlg'
  ClientWidth = 401
  ExplicitWidth = 407
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Top = 9
    Width = 379
    ExplicitTop = 9
    ExplicitWidth = 379
    inline frmHTML: THTMLTpltDlgFrame
      Left = 0
      Top = 31
      Width = 379
      Height = 250
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitTop = 31
      ExplicitWidth = 379
      ExplicitHeight = 250
      inherited pnlBrowser: TPanel
        Width = 379
        Height = 250
        ExplicitWidth = 379
        ExplicitHeight = 250
        inherited wbBrowser: TWebBrowser
          Width = 379
          Height = 250
          ExplicitTop = -7
          ExplicitWidth = 377
          ExplicitHeight = 273
          ControlData = {
            4C0000002C270000D71900000000000000000000000000000000000000000000
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
      Width = 379
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      DitherBackground = False
      EndMargin = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = ilCompilers
      StartMargin = 4
      SoftTop = True
      Style = tsSoftTabs
      TabPosition = tpTop
      OnChange = tsCompilersChange
      OnGetImageIndex = tsCompilersGetImageIndex
    end
  end
  inherited btnHelp: TButton
    Visible = False
  end
  object ilCompilers: TImageList
    Left = 16
    Top = 48
  end
  object alTabs: TActionList
    Left = 48
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
