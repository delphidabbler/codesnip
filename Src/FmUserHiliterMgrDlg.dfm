inherited UserHiliterMgrDlg: TUserHiliterMgrDlg
  Caption = 'User Defined Highlighters'
  ClientHeight = 1844
  ClientWidth = 2561
  Font.Height = -200
  ExplicitWidth = 2585
  ExplicitHeight = 1908
  PixelsPerInch = 168
  TextHeight = 265
  inherited bvlBottom: TBevel
    Left = 77
    Top = 2777
    Width = 3537
    Height = 23
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 2777
    ExplicitWidth = 3537
    ExplicitHeight = 23
  end
  inherited pnlBody: TPanel
    Left = 77
    Top = 77
    Width = 3537
    Height = 382
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 77
    ExplicitWidth = 3537
    ExplicitHeight = 382
    object lblNames: TLabel
      Left = 0
      Top = 0
      Width = 1966
      Height = 265
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
      Height = 303
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      IntegralHeight = True
      ItemHeight = 23
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
  inherited btnHelp: TButton
    Left = 2928
    Top = 2851
    Width = 702
    Height = 236
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
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
    Caption = 'Done'
    Default = False
    ModalResult = 2
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
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
