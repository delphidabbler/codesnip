inherited EasterEggForm: TEasterEggForm
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'EasterEggForm'
  ClientHeight = 1103
  ClientWidth = 949
  Color = clPurple
  TransparentColor = True
  TransparentColorValue = clPurple
  UseDockManager = True
  ParentFont = True
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  ExplicitWidth = 949
  ExplicitHeight = 1103
  PixelsPerInch = 168
  TextHeight = 30
  inline frmEasterEgg: TEasterEggFrame
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 949
    Height = 1103
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 949
    ExplicitHeight = 1103
    inherited pnlBrowser: TPanel
      Width = 949
      Height = 1103
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      ExplicitWidth = 949
      ExplicitHeight = 1103
      inherited wbBrowser: TWebBrowser
        Width = 949
        Height = 1103
        Margins.Left = 9
        Margins.Top = 9
        Margins.Right = 9
        Margins.Bottom = 9
        ExplicitWidth = 949
        ExplicitHeight = 1103
        ControlData = {
          4C0000000C380000244100000000000000000000000000000000000000000000
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
