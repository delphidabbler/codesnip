inherited DuplicateSnippetDlg: TDuplicateSnippetDlg
  Caption = 'DuplicateSnippetDlg'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 337
    Height = 273
    ExplicitWidth = 337
    ExplicitHeight = 273
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
      Width = 24
      Height = 13
      Caption = '&Title:'
      FocusControl = edDisplayName
    end
    object cbCategory: TComboBox
      Left = 0
      Top = 77
      Width = 337
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 222
    end
    object edDisplayName: TEdit
      Left = 0
      Top = 21
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 222
    end
    object chkEdit: TCheckBox
      Left = 0
      Top = 114
      Width = 337
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Edit in Snippets Editor'
      TabOrder = 2
      ExplicitWidth = 222
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
