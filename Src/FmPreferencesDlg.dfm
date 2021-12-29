inherited PreferencesDlg: TPreferencesDlg
  Left = 425
  Top = 138
  Caption = 'Preferences'
  ClientHeight = 421
  ClientWidth = 722
  ExplicitWidth = 728
  ExplicitHeight = 450
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 609
    Height = 329
    ExplicitWidth = 609
    ExplicitHeight = 329
    object pcMain: TPageControl
      Left = 163
      Top = 0
      Width = 446
      Height = 329
      Align = alRight
      MultiLine = True
      TabOrder = 1
      ExplicitLeft = 159
      ExplicitHeight = 377
    end
    object lbPages: TListBox
      Left = 0
      Top = 0
      Width = 153
      Height = 329
      Align = alLeft
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbPagesClick
      ExplicitHeight = 377
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
