inherited DuplicateSnippetDlg: TDuplicateSnippetDlg
  Caption = 'DuplicateSnippetDlg'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblUniqueName: TLabel
      Left = 0
      Top = 0
      Width = 338
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Unique name for duplicated snippet:'
      FocusControl = edUniqueName
    end
    object lblCategory: TLabel
      Left = 0
      Top = 182
      Width = 88
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Category:'
      FocusControl = cbCategory
    end
    object lblDisplayName: TLabel
      Left = 0
      Top = 84
      Width = 130
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Display name:'
      FocusControl = edDisplayName
    end
    object edUniqueName: TEdit
      Left = 0
      Top = 33
      Width = 388
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object cbCategory: TComboBox
      Left = 0
      Top = 215
      Width = 388
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edDisplayName: TEdit
      Left = 0
      Top = 117
      Width = 388
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object chkEdit: TCheckBox
      Left = 0
      Top = 280
      Width = 388
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Edit in Snippets Editor'
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
