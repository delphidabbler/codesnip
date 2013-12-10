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
    object lblTitle: TLabel
      Left = 0
      Top = 2
      Width = 24
      Height = 13
      Caption = '&Title:'
      FocusControl = edTitle
    end
    object edTitle: TEdit
      Left = 0
      Top = 21
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object chkEdit: TCheckBox
      Left = 0
      Top = 114
      Width = 337
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Edit in Snippets Editor'
      TabOrder = 1
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
