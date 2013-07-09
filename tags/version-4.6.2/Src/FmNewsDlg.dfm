inherited NewsDlg: TNewsDlg
  Left = 1267
  Top = 793
  Caption = 'CodeSnip News'
  ClientHeight = 387
  Position = poDesigned
  ExplicitWidth = 474
  ExplicitHeight = 415
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
        Action = actConfig
        Anchors = [akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
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
          ExplicitLeft = 0
          ExplicitTop = 0
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
    Action = actRSSFeed
    Caption = '&RSS Feed'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object ilActions: TImageList
    Left = 224
    Top = 192
  end
  object alMain: TActionList
    Images = ilActions
    Left = 168
    Top = 192
    object actRSSFeed: TAction
      Caption = '&RSS Feed'
      ImageIndex = 45
      OnExecute = actRSSFeedExecute
    end
    object actConfig: TAction
      Caption = '&Change...'
      OnExecute = actConfigExecute
    end
  end
end
