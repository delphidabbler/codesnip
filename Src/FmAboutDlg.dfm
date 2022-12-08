inherited AboutDlg: TAboutDlg
  Caption = 'About CodeSnip'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Left = 25
    Top = 25
    Width = 715
    Height = 477
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    ExplicitLeft = 25
    ExplicitTop = 25
    ExplicitWidth = 715
    ExplicitHeight = 477
    object bvlSeparator: TBevel
      Left = 0
      Top = 72
      Width = 715
      Height = 10
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Shape = bsSpacer
      ExplicitWidth = 716
    end
    object pcDetail: TPageControl
      Left = 0
      Top = 82
      Width = 715
      Height = 382
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsPaths
      Align = alTop
      TabOrder = 0
      OnMouseDown = pcDetailMouseDown
      object tsProgram: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'About The Program'
        inline frmProgram: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 401
          Height = 190
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 401
          ExplicitHeight = 190
          inherited pnlBrowser: TPanel
            Width = 401
            Height = 190
            ExplicitWidth = 401
            ExplicitHeight = 190
            inherited wbBrowser: TWebBrowser
              Width = 401
              Height = 190
              ExplicitWidth = 345
              ExplicitHeight = 133
              ControlData = {
                4C00000072290000A31300000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsDatabase: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'About The Database'
        ImageIndex = 1
        inline frmDatabase: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 401
          Height = 190
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 401
          ExplicitHeight = 190
          inherited pnlBrowser: TPanel
            Width = 401
            Height = 190
            ExplicitWidth = 401
            ExplicitHeight = 190
            inherited wbBrowser: TWebBrowser
              Width = 401
              Height = 190
              ExplicitWidth = 345
              ExplicitHeight = 133
              ControlData = {
                4C00000072290000A31300000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsPaths: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Paths && Files'
        ImageIndex = 2
        object btnViewAppConfig: TButton
          Left = 5
          Top = 252
          Width = 298
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'View Application Config File...'
          TabOrder = 1
          OnClick = btnViewAppConfigClick
        end
        object btnViewUserConfig: TButton
          Left = 336
          Top = 252
          Width = 298
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'View Per-User Config File...'
          TabOrder = 0
          OnClick = btnViewUserConfigClick
        end
      end
    end
    object pnlTitle: TPanel
      Left = 0
      Top = 0
      Width = 715
      Height = 72
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clBtnShadow
      TabOrder = 1
      inline frmTitle: THTMLTpltDlgFrame
        Left = 1
        Top = 1
        Width = 713
        Height = 70
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 713
        ExplicitHeight = 70
        inherited pnlBrowser: TPanel
          Width = 713
          Height = 70
          ExplicitWidth = 713
          ExplicitHeight = 70
          inherited wbBrowser: TWebBrowser
            Width = 713
            Height = 70
            ExplicitWidth = 713
            ExplicitHeight = 70
            ControlData = {
              4C0000001C2A0000220400000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
  end
end
