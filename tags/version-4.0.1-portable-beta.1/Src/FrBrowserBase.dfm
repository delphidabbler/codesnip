object BrowserBaseFrame: TBrowserBaseFrame
  Left = 0
  Top = 0
  Width = 316
  Height = 236
  TabOrder = 0
  TabStop = True
  OnEnter = FrameEnter
  object pnlBrowser: TPanel
    Left = 0
    Top = 0
    Width = 316
    Height = 236
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object wbBrowser: TWebBrowser
      Left = 0
      Top = 0
      Width = 316
      Height = 236
      Align = alClient
      TabOrder = 0
      ControlData = {
        4C000000A9200000641800000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
end
