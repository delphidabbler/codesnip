inherited ProxyServerDlg: TProxyServerDlg
  Caption = 'Proxy Server Configuration'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 299
    Height = 245
    ExplicitWidth = 299
    ExplicitHeight = 245
    object cbUseProxy: TCheckBox
      Left = 0
      Top = 0
      Width = 169
      Height = 17
      Caption = 'Use proxy server'
      TabOrder = 0
      OnClick = cbUseProxyClick
    end
    object gbProxy: TGroupBox
      Left = 0
      Top = 27
      Width = 299
      Height = 218
      Caption = 'Proxy Server Details'
      TabOrder = 1
      object lblIPAddress: TLabel
        Left = 11
        Top = 28
        Width = 88
        Height = 13
        Caption = 'Server IP Address:'
        FocusControl = edIPAddress
      end
      object lblPort: TLabel
        Left = 11
        Top = 59
        Width = 73
        Height = 13
        Caption = 'Port (0..65535):'
        FocusControl = edPort
      end
      object lblUserName: TLabel
        Left = 11
        Top = 89
        Width = 56
        Height = 13
        Caption = 'User Name:'
        FocusControl = edUserName
      end
      object lblPassword1: TLabel
        Left = 11
        Top = 119
        Width = 49
        Height = 13
        Caption = 'Password:'
        FocusControl = edPassword1
      end
      object lblPassword2: TLabel
        Left = 11
        Top = 150
        Width = 93
        Height = 13
        Caption = 'Re-enter Password:'
        FocusControl = edPassword2
      end
      object lblIPAddressReq: TLabel
        Left = 242
        Top = 29
        Width = 5
        Height = 16
        Caption = '*'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblPortReq: TLabel
        Left = 180
        Top = 58
        Width = 5
        Height = 16
        Caption = '*'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblReqSymbol: TLabel
        Left = 11
        Top = 185
        Width = 5
        Height = 16
        Caption = '*'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblReqExplain: TLabel
        Left = 22
        Top = 187
        Width = 72
        Height = 13
        Caption = '= required field.'
      end
      object edIPAddress: TEdit
        Left = 131
        Top = 27
        Width = 105
        Height = 21
        MaxLength = 15
        TabOrder = 0
        OnKeyPress = edIPAddressKeyPress
      end
      object edPort: TEdit
        Left = 131
        Top = 56
        Width = 43
        Height = 21
        MaxLength = 5
        TabOrder = 1
        OnKeyPress = edPortKeyPress
      end
      object edUserName: TEdit
        Left = 131
        Top = 85
        Width = 153
        Height = 21
        TabOrder = 2
      end
      object edPassword1: TEdit
        Left = 131
        Top = 115
        Width = 153
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
      object edPassword2: TEdit
        Left = 131
        Top = 146
        Width = 153
        Height = 21
        PasswordChar = '*'
        TabOrder = 4
      end
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
