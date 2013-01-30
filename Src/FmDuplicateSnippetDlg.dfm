inherited DuplicateSnippetDlg: TDuplicateSnippetDlg
  Caption = 'DuplicateSnippetDlg'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 200
    Height = 177
    ExplicitWidth = 200
    ExplicitHeight = 177
    object lblNewName: TLabel
      Left = 0
      Top = 0
      Width = 188
      Height = 13
      Caption = 'Enter a &name for the duplicated snippet:'
      FocusControl = edNewName
    end
    object lblCategory: TLabel
      Left = 0
      Top = 56
      Width = 181
      Height = 13
      Caption = 'Choose &category for duplicate snippet:'
      FocusControl = cbCategory
    end
    object edNewName: TEdit
      Left = 0
      Top = 19
      Width = 200
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 201
    end
    object cbCategory: TComboBox
      Left = 0
      Top = 75
      Width = 200
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
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
