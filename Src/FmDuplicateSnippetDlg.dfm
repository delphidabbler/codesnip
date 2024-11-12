inherited DuplicateSnippetDlg: TDuplicateSnippetDlg
  Caption = 'DuplicateSnippetDlg'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 222
    Height = 185
    ExplicitWidth = 222
    ExplicitHeight = 185
    object lblCategory: TLabel
      Left = 0
      Top = 58
      Width = 49
      Height = 13
      Caption = '&Category:'
      FocusControl = cbCategory
    end
    object lblDisplayName: TLabel
      Left = 0
      Top = 2
      Width = 36
      Height = 13
      Caption = '&Snippet'
      FocusControl = edDisplayName
    end
    object lblCollection: TLabel
      Left = 0
      Top = 116
      Width = 50
      Height = 13
      Caption = '&Collection:'
      FocusControl = cbCollection
    end
    object cbCategory: TComboBox
      Left = 0
      Top = 77
      Width = 222
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edDisplayName: TEdit
      Left = 0
      Top = 21
      Width = 222
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object chkEdit: TCheckBox
      Left = 0
      Top = 162
      Width = 222
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Edit in Snippets Editor'
      TabOrder = 2
    end
    object cbCollection: TComboBox
      Left = 0
      Top = 135
      Width = 222
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  inherited btnHelp: TButton
    Left = 313
    ExplicitLeft = 313
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
