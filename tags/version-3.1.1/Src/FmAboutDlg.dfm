inherited AboutDlg: TAboutDlg
  Left = 543
  Top = 188
  Caption = 'About CodeSnip'
  ClientHeight = 384
  ClientWidth = 485
  ExplicitWidth = 491
  ExplicitHeight = 410
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 273
    ExplicitHeight = 273
    object bvlSeparator: TBevel
      Left = 0
      Top = 41
      Width = 377
      Height = 6
      Align = alTop
      Shape = bsSpacer
      ExplicitWidth = 353
    end
    object pcDetail: TPageControl
      Left = 0
      Top = 47
      Width = 377
      Height = 161
      ActivePage = tsProgram
      Align = alTop
      TabOrder = 0
      object tsProgram: TTabSheet
        Caption = 'About The Program'
        inline frmProgram: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 369
          Height = 133
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 369
          ExplicitHeight = 133
          inherited pnlBrowser: TPanel
            Width = 369
            Height = 133
            ExplicitWidth = 369
            ExplicitHeight = 133
            inherited wbBrowser: TWebBrowser
              Width = 369
              Height = 133
              ExplicitWidth = 345
              ExplicitHeight = 133
              ControlData = {
                4C00000023260000BF0D00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsDatabase: TTabSheet
        Caption = 'About The Database'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inline frmDatabase: THTMLTpltDlgFrame
          Left = 0
          Top = 0
          Width = 345
          Height = 133
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 345
          ExplicitHeight = 133
          inherited pnlBrowser: TPanel
            Width = 345
            Height = 133
            ExplicitWidth = 345
            ExplicitHeight = 133
            inherited wbBrowser: TWebBrowser
              Width = 345
              Height = 133
              ExplicitWidth = 345
              ExplicitHeight = 133
              ControlData = {
                4C000000A8230000BF0D00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
    end
    object pnlTitle: TPanel
      Left = 0
      Top = 0
      Width = 377
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clBtnShadow
      TabOrder = 1
      inline frmTitle: THTMLTpltDlgFrame
        Left = 1
        Top = 1
        Width = 375
        Height = 39
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 375
        ExplicitHeight = 39
        inherited pnlBrowser: TPanel
          Width = 375
          Height = 39
          ExplicitWidth = 375
          ExplicitHeight = 39
          inherited wbBrowser: TWebBrowser
            Width = 375
            Height = 39
            ExplicitWidth = 351
            ExplicitHeight = 39
            ControlData = {
              4C000000C2260000080400000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
  end
  inherited btnHelp: TButton
    TabOrder = 3
  end
  inherited btnClose: TButton
    TabOrder = 2
  end
  object btnRegister: TButton
    Left = 8
    Top = 304
    Width = 113
    Height = 25
    Caption = '&Register CodeSnip...'
    TabOrder = 1
    OnClick = btnRegisterClick
  end
end
