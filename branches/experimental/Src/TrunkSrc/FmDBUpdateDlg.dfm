inherited DBUpdateDlg: TDBUpdateDlg
  Left = 350
  Top = 171
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Update From Web'
  ClientHeight = 344
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 474
  ExplicitHeight = 372
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 329
    Height = 137
    ExplicitWidth = 329
    ExplicitHeight = 137
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
      Width = 62
      Height = 13
      Cursor = crHandPoint
      Caption = 'details below'
      Visible = False
    end
    object lblHeadline: TLabel
      Left = 0
      Top = 0
      Width = 62
      Height = 13
      Alignment = taCenter
      Caption = 'lblHeadline'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
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
      Top = 47
      Width = 153
      Height = 41
      Caption = '&Update from Web'
      TabOrder = 0
      OnClick = btnDoUpdateClick
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  inherited btnClose: TButton
    TabOrder = 2
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
  object btnNews: TButton
    Left = 8
    Top = 304
    Width = 91
    Height = 25
    Caption = 'Latest &News...'
    TabOrder = 1
    OnClick = btnNewsClick
  end
end
