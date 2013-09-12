inherited DeleteCategoryDlg: TDeleteCategoryDlg
  Caption = 'Delete Category'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblErrorMsg: TLabel
      Left = 0
      Top = 144
      Width = 156
      Height = 13
      Caption = 'Non-empty category: can'#39't delete'
    end
    inline frmCategories: TCategoryListFrame
      Left = 0
      Top = 0
      Width = 320
      Height = 138
      TabOrder = 0
      TabStop = True
      ExplicitHeight = 138
      inherited lblCategories: TLabel
        Width = 60
        ExplicitWidth = 60
      end
      inherited lbCategories: TListBox
        Height = 108
        ExplicitHeight = 108
      end
    end
  end
  inherited btnOK: TButton
    Caption = '&Delete'
    OnClick = btnOKClick
  end
end
