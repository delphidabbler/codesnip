inherited CompErrorDlg: TCompErrorDlg
  Caption = 'Compiler Errors & Warnings'
  ClientHeight = 1844
  ClientWidth = 711
  Font.Height = -200
  ExplicitWidth = 735
  ExplicitHeight = 1908
  PixelsPerInch = 168
  TextHeight = 265
  inherited bvlBottom: TBevel
    Left = 77
    Top = 2777
    Width = 3537
    Height = 23
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 2777
    ExplicitWidth = 3537
    ExplicitHeight = 23
  end
  inherited pnlBody: TPanel
    Left = 77
    Top = 16
    Width = 663
    Height = 2637
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 16
    ExplicitWidth = 663
    ExplicitHeight = 2637
    inline frmHTML: THTMLTpltDlgFrame
      Left = 0
      Top = 31
      Width = 379
      Height = 250
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
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
      Width = 663
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
  inherited btnHelp: TButton
    Left = 2928
    Top = 2851
    Width = 702
    Height = 236
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    Visible = False
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
  end
  inherited btnClose: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
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
