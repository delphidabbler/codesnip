inherited AboutDlg: TAboutDlg
  Left = 543
  Top = 188
  Caption = 'About CodeSnip'
  ClientHeight = 384
  ClientWidth = 485
  ExplicitWidth = 491
  ExplicitHeight = 413
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 409
    Height = 273
    ExplicitWidth = 409
    ExplicitHeight = 273
    object bvlSeparator: TBevel
      Left = 0
      Top = 41
      Width = 409
      Height = 6
      Align = alTop
      Shape = bsSpacer
      ExplicitWidth = 353
    end
    object pcDetail: TPageControl
      Left = 0
      Top = 47
      Width = 409
      Height = 218
      ActivePage = tsVaults
      Align = alTop
      TabOrder = 0
      OnMouseDown = pcDetailMouseDown
      object tsProgram: TTabSheet
        Caption = 'About The Program'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inline frmProgram: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 401
          Height = 190
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
      object tsVaults: TTabSheet
        Caption = 'About Vaults'
        ImageIndex = 1
        DesignSize = (
          401
          190)
        object lblVaults: TLabel
          Left = 3
          Top = 3
          Width = 60
          Height = 13
          Caption = '&Select vault:'
          FocusControl = cbVaults
        end
        object cbVaults: TComboBox
          Left = 112
          Top = 0
          Width = 286
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = cbVaultsChange
        end
        object tvVaultInfo: TTreeView
          Left = 0
          Top = 27
          Width = 401
          Height = 163
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          Indent = 19
          ParentColor = True
          ReadOnly = True
          ShowRoot = False
          TabOrder = 1
        end
      end
      object tsPaths: TTabSheet
        Caption = 'Paths && Files'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          401
          190)
        object btnViewAppConfig: TButton
          Left = 3
          Top = 3
          Width = 170
          Height = 25
          Caption = 'View Application Config File...'
          TabOrder = 0
          OnClick = btnViewAppConfigClick
        end
        object btnViewUserConfig: TButton
          Left = 228
          Top = 3
          Width = 170
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'View Per-User Config File...'
          TabOrder = 1
          OnClick = btnViewUserConfigClick
        end
        object sbPaths: TScrollBox
          Left = 3
          Top = 34
          Width = 395
          Height = 153
          BevelInner = bvLowered
          BevelOuter = bvRaised
          BevelKind = bkFlat
          BorderStyle = bsNone
          Padding.Left = 3
          Padding.Top = 3
          Padding.Right = 3
          Padding.Bottom = 3
          TabOrder = 2
          TabStop = True
        end
      end
    end
    object pnlTitle: TPanel
      Left = 0
      Top = 0
      Width = 409
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clBtnShadow
      TabOrder = 1
      inline frmTitle: THTMLTpltDlgFrame
        Left = 1
        Top = 1
        Width = 407
        Height = 39
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 407
        ExplicitHeight = 39
        inherited pnlBrowser: TPanel
          Width = 407
          Height = 39
          ExplicitWidth = 407
          ExplicitHeight = 39
          inherited wbBrowser: TWebBrowser
            Width = 407
            Height = 39
            ExplicitWidth = 351
            ExplicitHeight = 39
            ControlData = {
              4C000000112A0000080400000000000000000000000000000000000000000000
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
