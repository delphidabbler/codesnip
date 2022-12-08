inherited SplashForm: TSplashForm
  BorderStyle = bsNone
  Caption = 'SplashForm'
  ClientHeight = 271
  ClientWidth = 569
  DefaultMonitor = dmMainForm
  ParentFont = True
  FormStyle = fsStayOnTop
  OnClose = FormClose
  ExplicitWidth = 569
  ExplicitHeight = 271
  PixelsPerInch = 168
  TextHeight = 30
  object pbMain: TPaintBox
    Left = 0
    Top = 0
    Width = 569
    Height = 271
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    OnPaint = pbMainPaint
  end
  object tmMinDisplay: TTimer
    Interval = 2000
    OnTimer = tmMinDisplayTimer
    Left = 32
    Top = 32
  end
end
