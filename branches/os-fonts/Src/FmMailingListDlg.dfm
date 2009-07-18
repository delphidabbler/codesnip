inherited MailingListDlg: TMailingListDlg
  Caption = 'Join Mailing List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 354
    Height = 217
    ExplicitWidth = 354
    ExplicitHeight = 217
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
        Top = 58
        Width = 105
        Height = 26
        AutoSize = False
        Caption = '&Email address:'#13#10'(required)'
        FocusControl = edEmail
      end
      object lblName: TLabel
        Left = 0
        Top = 102
        Width = 105
        Height = 26
        AutoSize = False
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
        WordWrap = True
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
        Left = 111
        Top = 55
        Width = 243
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkFlat
        BevelOuter = bvRaised
        BorderStyle = bsNone
        TabOrder = 0
      end
      object edName: TEdit
        Left = 111
        Top = 120
        Width = 243
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkFlat
        BevelOuter = bvRaised
        BorderStyle = bsNone
        TabOrder = 1
      end
      inline frmPrivacy: TFixedHTMLDlgFrame
        Left = 111
        Top = 82
        Width = 243
        Height = 26
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 111
        ExplicitTop = 82
        ExplicitWidth = 243
        ExplicitHeight = 26
        inherited pnlBrowser: TPanel
          Width = 243
          Height = 26
          ExplicitWidth = 243
          ExplicitHeight = 26
          inherited wbBrowser: TWebBrowser
            Width = 243
            Height = 26
            ExplicitWidth = 243
            ExplicitHeight = 26
            ControlData = {
              4C0000001D190000B00200000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
      inline frmMailList: TFixedHTMLDlgFrame
        Left = 0
        Top = 6
        Width = 355
        Height = 26
        TabOrder = 3
        TabStop = True
        ExplicitTop = 6
        ExplicitWidth = 355
        ExplicitHeight = 26
        inherited pnlBrowser: TPanel
          Width = 355
          Height = 26
          ExplicitWidth = 249
          ExplicitHeight = 26
          inherited wbBrowser: TWebBrowser
            Width = 355
            Height = 26
            ExplicitWidth = 249
            ExplicitHeight = 26
            ControlData = {
              4C000000BC190000B00200000000000000000000000000000000000000000000
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
