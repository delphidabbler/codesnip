inherited DuplicateSnippetDlg: TDuplicateSnippetDlg
  Caption = 'DuplicateSnippetDlg'
  ExplicitWidth = 474
  ExplicitHeight = 356
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 222
    Height = 185
    ExplicitWidth = 222
    ExplicitHeight = 185
    object lblUniqueName: TLabel
      Left = 0
      Top = 0
      Width = 173
      Height = 13
      Caption = '&Unique name for duplicated snippet:'
      FocusControl = edUniqueName
    end
    object lblCategory: TLabel
      Left = 0
      Top = 104
      Width = 49
      Height = 13
      Caption = '&Category:'
      FocusControl = cbCategory
    end
    object lblDisplayName: TLabel
      Left = 0
      Top = 48
      Width = 67
      Height = 13
      Caption = '&Display name:'
      FocusControl = edDisplayName
    end
    object edUniqueName: TEdit
      Left = 0
      Top = 19
      Width = 222
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 200
    end
    object cbCategory: TComboBox
      Left = 0
      Top = 123
      Width = 222
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edDisplayName: TEdit
      Left = 0
      Top = 67
      Width = 222
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 200
    end
    object chkEdit: TCheckBox
      Left = 0
      Top = 160
      Width = 222
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Edit in Snippets Editor'
      TabOrder = 3
      ExplicitWidth = 200
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
