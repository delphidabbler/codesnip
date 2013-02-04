inherited FavouritesDlg: TFavouritesDlg
  Caption = 'Favourites'
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object btnDisplay: TButton
      Left = 293
      Top = -1
      Width = 85
      Height = 25
      Action = actDisplay
      Caption = '&Display'
      TabOrder = 0
    end
    object btnDelete: TButton
      Left = 293
      Top = 30
      Width = 85
      Height = 25
      Action = actDelete
      Caption = '&Remove'
      TabOrder = 1
    end
    object btnDeleteAll: TButton
      Left = 293
      Top = 63
      Width = 85
      Height = 25
      Action = actDeleteAll
      Caption = '&Clear All'
      TabOrder = 2
    end
  end
  inherited btnClose: TButton
    OnClick = btnCloseClick
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
