inherited SplashForm: TSplashForm
  BorderStyle = bsNone
  Caption = 'SplashForm'
  ClientHeight = 155
  ClientWidth = 325
  DefaultMonitor = dmMainForm
  FormStyle = fsStayOnTop
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pbMain: TPaintBox
    Left = 0
    Top = 0
    Width = 325
    Height = 155
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
