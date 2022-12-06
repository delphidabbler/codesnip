inherited FavouritesDlg: TFavouritesDlg
  AlphaBlend = True
  Caption = 'Favourites'
  ClientHeight = 1018
  ClientWidth = 1466
  Font.Height = -114
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  ExplicitWidth = 1490
  ExplicitHeight = 1082
  PixelsPerInch = 168
  TextHeight = 152
  inherited bvlBottom: TBevel
    Left = 44
    Top = 1587
    Width = 2021
    Height = 13
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    ExplicitLeft = 44
    ExplicitTop = 1587
    ExplicitWidth = 2021
    ExplicitHeight = 13
  end
  object lblTransparency: TLabel [1]
    Left = 14
    Top = 541
    Width = 684
    Height = 152
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tr&ansparency:'
    FocusControl = tbTransparency
  end
  inherited pnlBody: TPanel
    Left = 44
    Top = 44
    Width = 2021
    Height = 1507
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    ExplicitLeft = 44
    ExplicitTop = 44
    ExplicitWidth = 2021
    ExplicitHeight = 1507
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
    Left = 1673
    Top = 1629
    Width = 401
    Height = 135
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    TabOrder = 3
    ExplicitLeft = 1673
    ExplicitTop = 1629
    ExplicitWidth = 401
    ExplicitHeight = 135
  end
  inherited btnClose: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    TabOrder = 2
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
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
