inherited DeleteUnusedTagsDlg: TDeleteUnusedTagsDlg
  Caption = 'Delete Unused Tags'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 377
    ExplicitWidth = 377
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 221
      Height = 13
      Caption = 'Choose which empty &tags should be removed:'
      FocusControl = clbTags
    end
    object clbTags: TCheckListBox
      Left = 0
      Top = 19
      Width = 299
      Height = 225
      OnClickCheck = clbTagsClickCheck
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
    end
    object btnUncheckAll: TButton
      Left = 0
      Top = 248
      Width = 75
      Height = 25
      Caption = '&Uncheck All'
      TabOrder = 1
      OnClick = btnUncheckAllClick
    end
    object btnCheckAll: TButton
      Left = 224
      Top = 248
      Width = 75
      Height = 25
      Caption = '&Check All'
      TabOrder = 2
      OnClick = btnCheckAllClick
    end
  end
  inherited btnHelp: TButton
    Visible = False
  end
  inherited btnOK: TButton
    Caption = '&Delete'
    OnClick = btnOKClick
  end
end
