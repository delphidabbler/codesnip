inherited DetailFrame: TDetailFrame
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 39
      Caption = 'Details'
      ExplicitWidth = 39
    end
  end
  inline frmDetailView: TDetailViewFrame
    Left = 0
    Top = 50
    Width = 318
    Height = 188
    Align = alClient
    TabOrder = 2
    TabStop = True
    ExplicitTop = 50
    ExplicitWidth = 318
    ExplicitHeight = 188
    inherited pnlBrowser: TPanel
      Width = 318
      Height = 188
      ExplicitWidth = 318
      ExplicitHeight = 188
      inherited wbBrowser: TWebBrowser
        Width = 318
        Height = 188
        ExplicitWidth = 318
        ExplicitHeight = 188
        ControlData = {
          4C000000DE2000006E1300000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object tcViews: TTabControl
    Left = 0
    Top = 27
    Width = 318
    Height = 23
    Align = alTop
    PopupMenu = mnuTabs
    TabOrder = 1
    OnChange = tcViewsChange
    OnDragDrop = tcViewsDragDrop
    OnDragOver = tcViewsDragOver
    OnMouseDown = tcViewsMouseDown
  end
  object mnuTabs: TPopupMenu
    Left = 144
    Top = 104
  end
end
