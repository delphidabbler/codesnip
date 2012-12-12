inherited EasterEggForm: TEasterEggForm
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'EasterEggForm'
  ClientHeight = 630
  ClientWidth = 542
  Color = clPurple
  TransparentColor = True
  TransparentColorValue = clPurple
  UseDockManager = True
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  ExplicitWidth = 542
  ExplicitHeight = 630
  PixelsPerInch = 96
  TextHeight = 13
  inline frmEasterEgg: TEasterEggFrame
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 542
    Height = 630
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 526
    ExplicitHeight = 617
    inherited pnlBrowser: TPanel
      Width = 542
      Height = 630
      ExplicitWidth = 526
      ExplicitHeight = 617
      inherited wbBrowser: TWebBrowser
        Width = 542
        Height = 630
        ExplicitWidth = 327
        ExplicitHeight = 329
        ControlData = {
          4C000000043800001D4100000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object timerReveal: TTimer
    Enabled = False
    Interval = 20
    Left = 176
    Top = 176
  end
end
