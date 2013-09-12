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
    Top = 66
    Width = 393
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Measurement '
    TabOrder = 1
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
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object gbDisplay: TGroupBox
    Left = 1
    Top = 3
    Width = 393
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Display '
    TabOrder = 0
    object lblOverviewTree: TLabel
      Left = 8
      Top = 24
      Width = 161
      Height = 13
      Caption = 'Start overview pane treeview as:'
    end
    object cbOverviewTree: TComboBox
      Left = 184
      Top = 20
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
end
