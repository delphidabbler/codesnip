object UserDataPathDlgProgressFrame: TUserDataPathDlgProgressFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 106
  ParentBackground = False
  TabOrder = 0
  object pnlBody: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 106
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 16
    ExplicitWidth = 288
    ExplicitHeight = 73
    object lblDescription: TLabel
      Left = 8
      Top = 8
      Width = 63
      Height = 13
      AutoSize = False
      Caption = 'lblDescription'
    end
    object prgProgress: TProgressBar
      Left = 8
      Top = 33
      Width = 272
      Height = 17
      TabOrder = 0
    end
  end
end
