inherited FindTextDlg: TFindTextDlg
  Caption = 'Find Text'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblFindText: TLabel
      Left = 0
      Top = 7
      Width = 109
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Text to find:'
      FocusControl = cbFindText
    end
    object lblDesc: TLabel
      Left = 0
      Top = 84
      Width = 527
      Height = 47
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'Separate the words to be found by spaces. Do not include punctua' +
        'tion.'
      WordWrap = True
    end
    object cbFindText: TComboBox
      Left = 0
      Top = 42
      Width = 527
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      OnChange = cbFindTextChange
    end
    object rgLogic: TRadioGroup
      Left = 0
      Top = 140
      Width = 250
      Height = 135
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search &logic'
      ItemIndex = 1
      Items.Strings = (
        'Find all words'
        'Find any word')
      TabOrder = 1
      TabStop = True
    end
    object gbOptions: TGroupBox
      Left = 277
      Top = 140
      Width = 250
      Height = 135
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Options'
      TabOrder = 2
      object cbCaseSensitive: TCheckBox
        Left = 14
        Top = 88
        Width = 207
        Height = 29
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Case sensitive'
        TabOrder = 1
      end
      object cbWholeWords: TCheckBox
        Left = 14
        Top = 37
        Width = 207
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Whole words only'
        TabOrder = 0
      end
    end
    object rgScope: TRadioGroup
      Left = 0
      Top = 285
      Width = 529
      Height = 135
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
