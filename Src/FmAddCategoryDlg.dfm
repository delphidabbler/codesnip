inherited AddCategoryDlg: TAddCategoryDlg
  Caption = 'Add Category'
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inline frmDescription: TCategoryDescEditFrame
      Left = 0
      Top = 0
      Width = 320
      Height = 240
      TabOrder = 0
      TabStop = True
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
