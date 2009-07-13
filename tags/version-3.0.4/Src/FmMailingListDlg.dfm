inherited MailingListDlg: TMailingListDlg
  Caption = 'Join Mailing List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 354
    Height = 217
    ExplicitWidth = 354
    ExplicitHeight = 217
    object pnlData: TPanel
      Left = 0
      Top = 0
      Width = 354
      Height = 217
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object lblEmail: TLabel
        Left = 0
        Top = 53
        Width = 68
        Height = 26
        Caption = '&Email address:'#13#10'(required)'
        FocusControl = edEmail
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblPrivacy: TLabel
        Left = 104
        Top = 80
        Width = 249
        Height = 26
        AutoSize = False
        Caption = 
          'Your email address will not be abused. See the privacy statement' +
          ' for details.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblName: TLabel
        Left = 0
        Top = 123
        Width = 92
        Height = 26
        Caption = '&Name or nickname:'#13#10'(optional)'
        FocusControl = edName
      end
      object lblSubmit: TLabel
        Left = 0
        Top = 175
        Width = 354
        Height = 27
        AutoSize = False
        Caption = 
          'To subscribe please ensure you are connected to the internet the' +
          'n press the "Subscribe" button.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblMailList: TLabel
        Left = 0
        Top = 0
        Width = 354
        Height = 43
        AutoSize = False
        Caption = 
          'Members of the CodeSnip mailing list receive information about u' +
          'pdates to this program and the online database. Learn more about' +
          ' the mailing list. To join please provide the information reques' +
          'ted below. '
        WordWrap = True
      end
      object lblMailListHelp: TLabel
        Left = 180
        Top = 13
        Width = 151
        Height = 13
        Cursor = crHandPoint
        Caption = 'Learn more about the mailing list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        Transparent = True
        OnClick = lblMailListHelpClick
      end
      object lblPrivacyHelp: TLabel
        Left = 104
        Top = 93
        Width = 83
        Height = 13
        Cursor = crHandPoint
        Caption = 'privacy statement'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        Transparent = True
        OnClick = lblPrivacyHelpClick
      end
      object lblSubscribing: TLabel
        Left = 0
        Top = 175
        Width = 354
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Subscribing...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      object edEmail: TEdit
        Left = 104
        Top = 56
        Width = 249
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkFlat
        BevelOuter = bvRaised
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object edName: TEdit
        Left = 104
        Top = 126
        Width = 249
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkFlat
        BevelOuter = bvRaised
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object pnlResult: TPanel
      Left = 0
      Top = 0
      Width = 354
      Height = 217
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      inline frmResult: THTMLTpltDlgFrame
        Left = 0
        Top = 0
        Width = 354
        Height = 217
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 354
        ExplicitHeight = 217
        inherited pnlBrowser: TPanel
          Width = 354
          Height = 217
          ExplicitWidth = 354
          ExplicitHeight = 217
          inherited wbBrowser: TWebBrowser
            Width = 354
            Height = 217
            ExplicitWidth = 354
            ExplicitHeight = 217
            ControlData = {
              4C000000962400006D1600000000000000000000000000000000000000000000
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
  object btnCancel: TButton
    Left = 224
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnSubmit: TButton
    Left = 144
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Subscribe'
    Default = True
    TabOrder = 1
    OnClick = btnSubmitClick
  end
end
