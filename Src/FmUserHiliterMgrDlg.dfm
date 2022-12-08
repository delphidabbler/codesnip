inherited UserHiliterMgrDlg: TUserHiliterMgrDlg
  Caption = 'User Defined Highlighters'
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblNames: TLabel
      Left = 0
      Top = 0
      Width = 207
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Available &Highlighters:'
      FocusControl = lbNames
    end
    object lbNames: TListBox
      Left = 0
      Top = 54
      Width = 380
      Height = 244
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      IntegralHeight = True
      ItemHeight = 30
      TabOrder = 0
      OnDblClick = lbNamesDblClick
    end
    object btnUse: TButton
      Left = 390
      Top = 54
      Width = 158
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actUse
      Default = True
      TabOrder = 1
    end
    object btnDelete: TButton
      Left = 390
      Top = 126
      Width = 158
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDelete
      TabOrder = 2
    end
  end
  inherited btnClose: TButton
    Caption = 'Done'
    Default = False
    ModalResult = 2
  end
  object alDlg: TActionList
    Left = 152
    Top = 120
    object actUse: TAction
      Caption = '&Use && Close'
      OnExecute = actUseExecute
      OnUpdate = actUseUpdate
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
  end
end
