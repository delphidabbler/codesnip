inherited FindTextDlg: TFindTextDlg
  Caption = 'Find Text'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 357
    Height = 137
    ExplicitWidth = 357
    ExplicitHeight = 137
    object lblFindText: TLabel
      Left = 0
      Top = 4
      Width = 56
      Height = 13
      Caption = '&Text to find:'
      FocusControl = cbFindText
    end
    object lblDesc: TLabel
      Left = 76
      Top = 24
      Width = 281
      Height = 27
      AutoSize = False
      Caption = 
        'Separate the words to be found by spaces. Do not include punctua' +
        'tion.'
      WordWrap = True
    end
    object cbFindText: TComboBox
      Left = 76
      Top = 0
      Width = 281
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbFindTextChange
    end
    object rgLogic: TRadioGroup
      Left = 76
      Top = 56
      Width = 137
      Height = 77
      Caption = 'Search logic'
      ItemIndex = 1
      Items.Strings = (
        'Fi&nd all words'
        'Find any wo&rd')
      TabOrder = 1
      TabStop = True
    end
    object gbOptions: TGroupBox
      Left = 220
      Top = 56
      Width = 137
      Height = 77
      Caption = 'Options'
      TabOrder = 2
      object cbCaseSensitive: TCheckBox
        Left = 8
        Top = 50
        Width = 118
        Height = 17
        Caption = '&Case sensitive'
        TabOrder = 1
      end
      object cbWholeWords: TCheckBox
        Left = 8
        Top = 21
        Width = 118
        Height = 17
        Caption = '&Whole words only'
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
