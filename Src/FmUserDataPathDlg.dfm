inherited UserDataPathDlg: TUserDataPathDlg
  Caption = 'Move User Database'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblPath: TLabel
      Left = 0
      Top = 59
      Width = 98
      Height = 13
      Caption = 'New database &path:'
      FocusControl = edPath
    end
    object lblInstructions: TLabel
      Left = 0
      Top = 0
      Width = 358
      Height = 53
      AutoSize = False
      Caption = 
        'Use this dialogue box to move the user database to a new directo' +
        'ry. You must either select an empty directory or create a new on' +
        'e.'
      WordWrap = True
    end
    object edPath: TEdit
      Left = 0
      Top = 78
      Width = 325
      Height = 21
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 331
      Top = 78
      Width = 27
      Height = 21
      Action = actBrowse
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object btnMove: TButton
      Left = 0
      Top = 152
      Width = 153
      Height = 41
      Action = actMove
      TabOrder = 3
    end
    object btnDefaultPath: TButton
      Left = 0
      Top = 105
      Width = 153
      Height = 25
      Action = actDefaultPath
      TabOrder = 2
    end
  end
  object alDlg: TActionList
    Left = 224
    Top = 176
    object actBrowse: TAction
      Caption = '...'
      OnExecute = actBrowseExecute
    end
    object actDefaultPath: TAction
      Caption = '&Restore Default Path'
      OnExecute = actDefaultPathExecute
      OnUpdate = actDefaultPathUpdate
    end
    object actMove: TAction
      Caption = '&Move Database'
      OnExecute = actMoveExecute
      OnUpdate = actMoveUpdate
    end
  end
end
