inherited DeleteCategoryDlg: TDeleteCategoryDlg
  Caption = 'Delete Category'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblErrorMsg: TLabel
      Left = 0
      Top = 252
      Width = 311
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Non-empty category: can'#39't delete'
    end
    inline frmCategories: TCategoryListFrame
      Left = 0
      Top = 0
      Width = 320
      Height = 138
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 320
      ExplicitHeight = 138
      inherited lbCategories: TListBox
        Height = 94
        ExplicitHeight = 94
      end
    end
  end
  inherited btnOK: TButton
    Caption = '&Delete'
    OnClick = btnOKClick
  end
end
