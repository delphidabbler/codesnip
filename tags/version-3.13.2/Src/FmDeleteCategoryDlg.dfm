inherited DeleteCategoryDlg: TDeleteCategoryDlg
  Caption = 'Delete Category'
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inline frmCategories: TCategoryListFrame
      Left = 0
      Top = 0
      Width = 320
      Height = 240
      TabOrder = 0
      TabStop = True
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
