inherited FavouritesDlg: TFavouritesDlg
  AlphaBlend = True
  Caption = 'Favourites'
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  PixelsPerInch = 168
  TextHeight = 30
  object lblTransparency: TLabel [1]
    Left = 14
    Top = 541
    Width = 126
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tr&ansparency:'
    FocusControl = tbTransparency
  end
  inherited pnlBody: TPanel
    object btnDisplay: TButton
      Left = 513
      Top = -2
      Width = 149
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDisplay
      Caption = '&Display'
      TabOrder = 1
    end
    object btnDelete: TButton
      Left = 513
      Top = 53
      Width = 149
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDelete
      Caption = '&Remove'
      TabOrder = 2
    end
    object btnDeleteAll: TButton
      Left = 513
      Top = 110
      Width = 149
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDeleteAll
      Caption = '&Clear All'
      TabOrder = 3
    end
    object chkNewTab: TCheckBox
      Left = 0
      Top = 457
      Width = 366
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Open favourites in new &tabs'
      TabOrder = 0
    end
  end
  inherited btnHelp: TButton
    TabOrder = 3
  end
  inherited btnClose: TButton
    TabOrder = 2
  end
  object tbTransparency: TTrackBar
    Left = 166
    Top = 532
    Width = 175
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Max = 255
    Min = 128
    Position = 160
    PositionToolTip = ptBottom
    SelEnd = 255
    SelStart = 128
    ShowSelRange = False
    TabOrder = 1
    ThumbLength = 35
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
