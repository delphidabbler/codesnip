inherited GeneralPrefsFrame: TGeneralPrefsFrame
  Width = 397
  Height = 311
  ExplicitWidth = 397
  ExplicitHeight = 311
  DesignSize = (
    397
    311)
  object gpMeasurement: TGroupBox
    Left = 0
    Top = 0
    Width = 393
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Measurement '
    TabOrder = 0
    object lblUnits: TLabel
      Left = 8
      Top = 24
      Width = 157
      Height = 13
      Caption = 'Preferred units of measurement:'
    end
    object cbUnits: TComboBox
      Left = 168
      Top = 20
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
end
