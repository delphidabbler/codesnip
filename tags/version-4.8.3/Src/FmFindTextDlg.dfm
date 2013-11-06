inherited FindTextDlg: TFindTextDlg
  Caption = 'Find Text'
  ExplicitWidth = 474
  ExplicitHeight = 356
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 385
    Height = 257
    ExplicitWidth = 385
    ExplicitHeight = 257
    object lblFindText: TLabel
      Left = 0
      Top = 4
      Width = 60
      Height = 13
      Caption = '&Text to find:'
      FocusControl = cbFindText
    end
    object lblDesc: TLabel
      Left = 0
      Top = 48
      Width = 301
      Height = 27
      AutoSize = False
      Caption = 
        'Separate the words to be found by spaces. Do not include punctua' +
        'tion.'
      WordWrap = True
    end
    object cbFindText: TComboBox
      Left = 0
      Top = 24
      Width = 301
      Height = 21
      TabOrder = 0
      OnChange = cbFindTextChange
    end
    object rgLogic: TRadioGroup
      Left = 0
      Top = 80
      Width = 143
      Height = 77
      Caption = 'Search &logic'
      ItemIndex = 1
      Items.Strings = (
        'Find all words'
        'Find any word')
      TabOrder = 1
      TabStop = True
    end
    object gbOptions: TGroupBox
      Left = 158
      Top = 80
      Width = 143
      Height = 77
      Caption = '&Options'
      TabOrder = 2
      object cbCaseSensitive: TCheckBox
        Left = 8
        Top = 50
        Width = 118
        Height = 17
        Caption = 'Case sensitive'
        TabOrder = 1
      end
      object cbWholeWords: TCheckBox
        Left = 8
        Top = 21
        Width = 118
        Height = 17
        Caption = 'Whole words only'
        TabOrder = 0
      end
    end
    object rgScope: TRadioGroup
      Left = 0
      Top = 163
      Width = 302
      Height = 77
      Caption = 'Search &scope'
      ItemIndex = 0
      Items.Strings = (
        'Refine existing search'
        'Search whole database (resets current search)')
      TabOrder = 3
      TabStop = True
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
