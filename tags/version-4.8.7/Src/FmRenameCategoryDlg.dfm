inherited RenameCategoryDlg: TRenameCategoryDlg
  Caption = 'Rename Category'
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
    end
    inline frmDescription: TCategoryDescEditFrame
      Left = 0
      Top = 120
      Width = 320
      Height = 240
      TabOrder = 1
      TabStop = True
      ExplicitTop = 120
      inherited lblError: TLabel
        Width = 108
        ExplicitWidth = 108
      end
    end
  end
  inherited btnOK: TButton
    Left = 151
    Caption = '&Rename'
    OnClick = btnOKClick
    ExplicitLeft = 151
  end
end
