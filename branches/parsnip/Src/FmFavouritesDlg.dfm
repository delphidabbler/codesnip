inherited FavouritesDlg: TFavouritesDlg
  AlphaBlend = True
  Caption = 'Favourites'
  ClientWidth = 569
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  ExplicitWidth = 575
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  object lblTransparency: TLabel [1]
    Left = 8
    Top = 309
    Width = 70
    Height = 13
    Caption = 'Tr&ansparency:'
    FocusControl = tbTransparency
  end
  inherited pnlBody: TPanel
    Width = 473
    ExplicitWidth = 473
    object btnDisplay: TButton
      Left = 370
      Top = -1
      Width = 85
      Height = 25
      Action = actDisplay
      Caption = '&Display'
      TabOrder = 1
    end
    object btnDelete: TButton
      Left = 370
      Top = 30
      Width = 85
      Height = 25
      Action = actDelete
      Caption = '&Remove'
      TabOrder = 2
    end
    object btnDeleteAll: TButton
      Left = 370
      Top = 63
      Width = 85
      Height = 25
      Action = actDeleteAll
      Caption = '&Clear All'
      TabOrder = 3
    end
    object chkNewTab: TCheckBox
      Left = 0
      Top = 261
      Width = 209
      Height = 17
      Caption = 'Open favourites in new &tabs'
      TabOrder = 0
    end
    object lbFavs: TListBox
      Left = 0
      Top = 0
      Width = 360
      Height = 240
      ItemHeight = 13
      TabOrder = 4
      OnDblClick = lbFavsDblClick
    end
  end
  inherited btnHelp: TButton
    TabOrder = 3
  end
  inherited btnClose: TButton
    TabOrder = 2
  end
  object tbTransparency: TTrackBar
    Left = 95
    Top = 304
    Width = 100
    Height = 25
    Max = 255
    Min = 128
    Position = 160
    PositionToolTip = ptBottom
    SelEnd = 255
    SelStart = 128
    ShowSelRange = False
    TabOrder = 1
    TickStyle = tsNone
    OnChange = tbTransparencyChange
    OnKeyDown = tbTransparencyKeyDown
    OnKeyUp = tbTransparencyKeyUp
  end
  object alDlg: TActionList
    Left = 416
    Top = 152
    object actDelete: TAction
      Caption = '&Remove Favourite'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actDisplay: TAction
      Caption = '&Display Favourite'
      OnExecute = actDisplayExecute
      OnUpdate = actDisplayUpdate
    end
    object actDeleteAll: TAction
      Caption = '&Clear All Favourites'
      OnExecute = actDeleteAllExecute
      OnUpdate = actDeleteAllUpdate
    end
  end
end
