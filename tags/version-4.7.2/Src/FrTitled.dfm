object TitledFrame: TTitledFrame
  Left = 0
  Top = 0
  Width = 318
  Height = 238
  TabOrder = 0
  TabStop = True
  object pnlTitle: TPanel
    Left = 0
    Top = 0
    Width = 318
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTitle: TLabel
      Left = 4
      Top = 6
      Width = 39
      Height = 13
      Caption = 'lblTitle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bvlTop: TBevel
      Left = 0
      Top = 23
      Width = 318
      Height = 4
      Align = alBottom
      Shape = bsTopLine
    end
  end
end
