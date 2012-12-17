object CategoryDescEditFrame: TCategoryDescEditFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  TabStop = True
  OnEnter = FrameEnter
  object lblError: TLabel
    Left = 0
    Top = 72
    Width = 115
    Height = 13
    Caption = 'Category already exists'
  end
  object lblDescription: TLabel
    Left = 0
    Top = 0
    Width = 65
    Height = 13
    Caption = 'edDescription'
    FocusControl = edDescription
  end
  object edDescription: TEdit
    Left = 0
    Top = 32
    Width = 320
    Height = 21
    TabOrder = 0
    OnChange = edDescriptionChange
  end
end
