inherited DetailFrame: TDetailFrame
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 40
      Caption = 'Details'
      ExplicitWidth = 40
    end
  end
  object pcDetail: TPageControl
    Left = 0
    Top = 27
    Width = 318
    Height = 211
    Align = alClient
    TabOrder = 1
    OnChange = pcDetailChange
  end
end
