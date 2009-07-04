inherited UpdateDlg: TUpdateDlg
  Left = 350
  Top = 171
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Update From Web'
  ClientHeight = 344
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 329
    Height = 273
    ExplicitWidth = 329
    ExplicitHeight = 273
    object lblUpdateFromWeb: TLabel
      Left = 0
      Top = 0
      Width = 329
      Height = 41
      Align = alTop
      AutoSize = False
      Caption = 
        'To download or update the code snippets database please connect ' +
        'to the internet and then click the "Update from Web" button.'
      WordWrap = True
    end
    object lblError: TLabel
      Left = 0
      Top = 0
      Width = 61
      Height = 13
      Cursor = crHandPoint
      Caption = 'details below'
      Visible = False
    end
    object lblHeadline: TLabel
      Left = 0
      Top = 0
      Width = 64
      Height = 13
      Alignment = taCenter
      Caption = 'lblHeadline'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object edProgress: TMemo
      Left = 0
      Top = 19
      Width = 329
      Height = 112
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      Visible = False
    end
    object btnDoUpdate: TButton
      Left = 88
      Top = 56
      Width = 153
      Height = 41
      Caption = '&Update from Web'
      TabOrder = 0
      OnClick = btnDoUpdateClick
    end
    inline frmNews: TNewsFrame
      Left = 0
      Top = 136
      Width = 329
      Height = 137
      TabOrder = 2
      TabStop = True
      ExplicitTop = 136
      ExplicitWidth = 329
      ExplicitHeight = 137
      inherited sbPrev: TSpeedButton
        Top = 115
        ExplicitTop = 115
      end
      inherited sbNext: TSpeedButton
        Left = 306
        Top = 115
        ExplicitLeft = 306
        ExplicitTop = 115
      end
      inherited frmDisplay: TNewsHTMLFrame
        Width = 329
        Height = 114
        ExplicitWidth = 329
        ExplicitHeight = 114
        inherited pnlBrowser: TPanel
          Width = 329
          Height = 114
          ExplicitWidth = 329
          ExplicitHeight = 114
          inherited wbBrowser: TWebBrowser
            Width = 327
            Height = 112
            ExplicitWidth = 327
            ExplicitHeight = 112
            ControlData = {
              4C000000CC210000930B00000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 120
    Top = 304
    Width = 91
    Height = 25
    Cancel = True
    Caption = 'Cancel Update'
    TabOrder = 3
    OnClick = btnCancelClick
  end
end
