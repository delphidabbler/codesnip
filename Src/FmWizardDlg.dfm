inherited WizardDlg: TWizardDlg
  Left = 411
  Top = 115
  Caption = 'WizardDlg'
  ClientHeight = 407
  ClientWidth = 559
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object pnlHead: TPanel
      Left = 0
      Top = 0
      Width = 377
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
      object lblHead: TLabel
        Left = 8
        Top = 9
        Width = 56
        Height = 16
        Caption = 'lblHead'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object pcWizard: TPageControl
      Left = 0
      Top = 33
      Width = 377
      Height = 248
      Align = alClient
      Style = tsButtons
      TabOrder = 1
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  object btnBack: TButton
    Left = 72
    Top = 304
    Width = 75
    Height = 25
    Caption = '&< Back'
    Enabled = False
    TabOrder = 1
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 152
    Top = 304
    Width = 75
    Height = 25
    Caption = 'btnNext'
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 232
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
