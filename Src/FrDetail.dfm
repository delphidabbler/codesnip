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
    ActivePage = tsCompiler
    Align = alClient
    TabOrder = 1
    OnChange = pcDetailChange
    OnMouseDown = pcDetailMouseDown
    object tsInfo: TTabSheet
      Caption = 'Information'
      inline frmInfo: TInfoFrame
        Left = 0
        Top = 0
        Width = 310
        Height = 183
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 310
        ExplicitHeight = 183
        inherited pnlBrowser: TPanel
          Width = 310
          Height = 183
          ExplicitWidth = 310
          ExplicitHeight = 183
          inherited wbBrowser: TWebBrowser
            Width = 310
            Height = 183
            ExplicitWidth = 312
            ExplicitHeight = 185
            ControlData = {
              4C0000000A200000EA1200000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
    object tsCompiler: TTabSheet
      Caption = 'Compiler Check'
      ImageIndex = 1
      inline frmCompCheck: TCompCheckFrame
        Left = 0
        Top = 0
        Width = 310
        Height = 183
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 310
        ExplicitHeight = 183
        inherited pnlBrowser: TPanel
          Width = 310
          Height = 183
          ExplicitWidth = 310
          ExplicitHeight = 183
          inherited wbBrowser: TWebBrowser
            Width = 310
            Height = 183
            ExplicitWidth = 312
            ExplicitHeight = 185
            ControlData = {
              4C0000000A200000EA1200000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126200000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
  end
end
