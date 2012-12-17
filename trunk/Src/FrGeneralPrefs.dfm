inherited GeneralPrefsFrame: TGeneralPrefsFrame
  Width = 397
  Height = 311
  ExplicitWidth = 397
  ExplicitHeight = 311
  DesignSize = (
    397
    311)
  object gbMeasurement: TGroupBox
    Left = 1
    Top = 3
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
      Caption = 'Preferred units of &measurement:'
      FocusControl = cbUnits
    end
    object cbUnits: TComboBox
      Left = 168
      Top = 20
      Width = 113
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
end
