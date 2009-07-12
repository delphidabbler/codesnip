inherited FindTextDlg: TFindTextDlg
  Caption = 'Find Text'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 353
    Height = 137
    ExplicitWidth = 353
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
      Left = 72
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
      Left = 72
      Top = 0
      Width = 281
      Height = 21
      ItemHeight = 0
      TabOrder = 0
      OnChange = cbFindTextChange
    end
    object rgLogic: TRadioGroup
      Left = 72
      Top = 56
      Width = 129
      Height = 81
      Caption = 'Search logic'
      ItemIndex = 1
      Items.Strings = (
        'Fi&nd all words'
        'Find any wo&rd')
      TabOrder = 1
      TabStop = True
    end
    object gbOptions: TGroupBox
      Left = 224
      Top = 56
      Width = 129
      Height = 81
      Caption = 'Options'
      TabOrder = 2
      object cbCaseSensitive: TCheckBox
        Left = 8
        Top = 52
        Width = 110
        Height = 17
        Caption = '&Case sensitive'
        TabOrder = 1
      end
      object cbWholeWords: TCheckBox
        Left = 8
        Top = 21
        Width = 109
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
