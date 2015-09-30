inherited TagsSearchDlg: TTagsSearchDlg
  Caption = 'Find Tags'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblTags: TLabel
      Left = 0
      Top = 8
      Width = 125
      Height = 13
      Caption = 'Choose one or more &tags:'
      FocusControl = clbTags
    end
    object clbTags: TCheckListBox
      Left = 0
      Top = 24
      Width = 336
      Height = 160
      OnClickCheck = clbTagsClickCheck
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
    end
    object rgLogic: TRadioGroup
      Left = 0
      Top = 190
      Width = 165
      Height = 82
      Caption = 'Search &logic'
      ItemIndex = 1
      Items.Strings = (
        'Find all tags'
        'Find any tag')
      TabOrder = 1
      TabStop = True
    end
    object rgScope: TRadioGroup
      Left = 171
      Top = 190
      Width = 165
      Height = 82
      Caption = 'Search &scope'
      ItemIndex = 0
      Items.Strings = (
        'Refine existing search'
        'Search whole database')
      TabOrder = 2
      TabStop = True
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
