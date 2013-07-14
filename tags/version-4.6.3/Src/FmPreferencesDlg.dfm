inherited PreferencesDlg: TPreferencesDlg
  Left = 425
  Top = 138
  Caption = 'Preferences'
  ClientHeight = 421
  ClientWidth = 462
  ExplicitWidth = 468
  ExplicitHeight = 447
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 446
    Height = 377
    ExplicitWidth = 446
    ExplicitHeight = 377
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 446
      Height = 377
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = pcMainChange
      OnChanging = pcMainChanging
      OnMouseDown = pcMainMouseDown
    end
  end
  inherited btnOK: TButton
    Left = 151
    Top = 303
    OnClick = btnOKClick
    ExplicitLeft = 151
    ExplicitTop = 303
  end
end
