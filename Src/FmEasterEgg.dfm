inherited EasterEggForm: TEasterEggForm
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderStyle = bsNone
  Caption = 'EasterEggForm'
  ClientHeight = 550
  ClientWidth = 380
  Color = clPurple
  TransparentColor = True
  TransparentColorValue = clPurple
  UseDockManager = True
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  ExplicitWidth = 380
  ExplicitHeight = 550
  PixelsPerInch = 96
  TextHeight = 13
  inline frmEasterEgg: TEasterEggFrame
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 380
    Height = 550
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 380
    ExplicitHeight = 550
    inherited pnlBrowser: TPanel
      Width = 380
      Height = 550
      ExplicitWidth = 380
      ExplicitHeight = 550
      inherited wbBrowser: TWebBrowser
        Width = 380
        Height = 550
        ExplicitWidth = 327
        ExplicitHeight = 329
        ControlData = {
          4C00000046270000D83800000000000000000000000000000000000000000000
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
