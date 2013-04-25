inherited UserHiliterMgrDlg: TUserHiliterMgrDlg
  Caption = 'User Defined Highlighters'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 218
    ExplicitHeight = 218
    object lblNames: TLabel
      Left = 0
      Top = 0
      Width = 106
      Height = 13
      Caption = 'Available &Highlighters:'
      FocusControl = lbNames
    end
    object lbNames: TListBox
      Left = 0
      Top = 31
      Width = 217
      Height = 173
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbNamesDblClick
    end
    object btnUse: TButton
      Left = 223
      Top = 31
      Width = 90
      Height = 25
      Action = actUse
      Default = True
      TabOrder = 1
    end
    object btnDelete: TButton
      Left = 223
      Top = 72
      Width = 90
      Height = 25
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
