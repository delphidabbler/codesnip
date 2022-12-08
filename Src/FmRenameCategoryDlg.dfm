inherited RenameCategoryDlg: TRenameCategoryDlg
  Caption = 'Rename Category'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    inline frmCategories: TCategoryListFrame
      Left = 0
      Top = 0
      Width = 320
      Height = 240
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 320
      ExplicitHeight = 240
    end
    inline frmDescription: TCategoryDescEditFrame
      Left = 0
      Top = 120
      Width = 320
      Height = 240
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      TabStop = True
      ExplicitTop = 120
      ExplicitWidth = 320
      ExplicitHeight = 240
    end
  end
  inherited btnOK: TButton
    Caption = '&Rename'
    OnClick = btnOKClick
  end
end
