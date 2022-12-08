inherited WizardDlg: TWizardDlg
  Caption = 'WizardDlg'
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object pnlHead: TPanel
      Left = 0
      Top = 0
      Width = 957
      Height = 71
      Align = alTop
      TabOrder = 0
      object lblHead: TLabel
        Left = 14
        Top = 16
        Width = 94
        Height = 36
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'lblHead'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -26
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object pcWizard: TPageControl
      Left = 0
      Top = 71
      Width = 957
      Height = 560
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Style = tsButtons
      TabOrder = 1
      ExplicitTop = 49
      ExplicitHeight = 582
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  object btnNext: TButton
    Left = 558
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'btnNext'
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 699
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnBack: TButton
    Left = 417
    Top = 708
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&< Back'
    Enabled = False
    TabOrder = 1
    OnClick = btnBackClick
  end
end
