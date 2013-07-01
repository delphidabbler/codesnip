inherited NewsDlg: TNewsDlg
  Left = 1267
  Top = 793
  Caption = 'CodeSnip News'
  ClientHeight = 387
  Position = poDesigned
  ExplicitWidth = 474
  ExplicitHeight = 413
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 441
    Height = 353
    ExplicitWidth = 441
    ExplicitHeight = 353
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 441
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        441
        41)
      object lblDays: TLabel
        Left = 0
        Top = 0
        Width = 34
        Height = 13
        Anchors = [akLeft]
        Caption = 'lblDays'
      end
      object btnConfig: TButton
        Left = 366
        Top = 10
        Width = 75
        Height = 25
        Hint = 'Change number of days'#13#10'of news displayed'
        Anchors = [akRight]
        Caption = 'Change...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnConfigClick
      end
    end
    inline frmHTML: TRSSNewsFrame
      Left = 0
      Top = 41
      Width = 441
      Height = 312
      Align = alClient
      TabOrder = 1
      TabStop = True
      ExplicitTop = 41
      ExplicitWidth = 441
      ExplicitHeight = 312
      inherited pnlBrowser: TPanel
        Width = 441
        Height = 312
        ExplicitWidth = 441
        ExplicitHeight = 312
        inherited wbBrowser: TWebBrowser
          Width = 439
          Height = 310
          ExplicitWidth = 439
          ExplicitHeight = 310
          ControlData = {
            4C0000005F2D00000A2000000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
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
  object btnRSSFeed: TBitBtn
    Left = 8
    Top = 304
    Width = 89
    Height = 25
    Hint = 'Display RSS feed'
    Caption = 'RSS Feed'
    DoubleBuffered = True
    Glyph.Data = {
      E6010000424DE60100000000000036000000280000000C0000000C0000000100
      180000000000B0010000C40E0000C40E000000000000000000002972D93586E8
      398FF13C8CF13A86EE3980EB377BE83676E53572E3346EE13072DE2972D93587
      E93A9DFB3897F7368EF33486EF327EEB3076E72E6FE32C69E02B63DD2A5EDB30
      72DE3F99F7399BFACEE7FEF2F8FE4F9CF44D95F1FFFFFF8AB1F17DA6EEFFFFFF
      5380E3306ADF3E96F53898F8F3F9FFFFFFFF83BDFA81B9F8FFFFFF659DF098BC
      F4FFFFFF2C69E03572E33D93F43794F66AB1F983C0FC46A3FBE6F2FEE6F1FD34
      86EFE5EFFDD8E5FA2E6FE33676E53C8FF2368FF3449BF683BFFBE6F3FEFFFFFF
      76B7FA75B2F7FFFFFF8BB6F33076E7377BE83B8BF0348AF1FFFFFFFFFFFFE6F2
      FE77BAFB5FAFFBF3F9FFE6F1FD408DF0327EEB3980EB3A87EE3385EE9AC4F868
      ABF63794F676B8FAF3F9FFF3F9FF5DABF9368EF33486EF3A86EE3983EB3180EB
      66A4F39AC5F8D9EAFDFFFFFFE6F2FE5EAEFB3A9DFB3897F7368EF33C8CF1337A
      E63079E8FFFFFFFFFFFFE6F0FD9AC7F9449BF63898F8399BFA3A9DFB3897F73D
      92F43178E12E73E5639AED3E87EC3385EE348AF1368FF33794F63898F8399BFA
      3A9DFB3586E82972D93178E1377EE93983EB3A87EE3B8BF03C8FF23D93F43E96
      F53B95F53587E92972D9}
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btnRSSFeedClick
  end
end
