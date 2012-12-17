object CategoryListFrame: TCategoryListFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  TabStop = True
  OnEnter = FrameEnter
  object lblCategories: TLabel
    Left = 0
    Top = 0
    Width = 62
    Height = 13
    Caption = 'lblCategories'
    FocusControl = lbCategories
  end
  object lbCategories: TListBox
    Left = 0
    Top = 19
    Width = 320
    Height = 95
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbCategoriesClick
  end
end
