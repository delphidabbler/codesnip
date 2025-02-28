inherited UserDataPathDlg: TUserDataPathDlg
  Caption = 'Move Collection Data'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Top = 9
    Height = 329
    ExplicitTop = 9
    ExplicitHeight = 329
    object lblInstructions: TLabel
      Left = 0
      Top = 0
      Width = 377
      Height = 25
      AutoSize = False
      Caption = 
        'Use this dialogue box to move a collection'#39's data to a new direc' +
        'tory.'#13#10'Choose the collection whose data you wish to move then en' +
        'ter the directory you wish to move the data to.'
      WordWrap = True
    end
    object lblWarning: TLabel
      Left = 0
      Top = 31
      Width = 377
      Height = 34
      AutoSize = False
      Caption = 
        'You are strongly advised to make a backup of the collection befo' +
        're continuing.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object lblPath: TLabel
      Left = 0
      Top = 125
      Width = 217
      Height = 13
      Caption = 'Enter the full path of the new data &directory:'
      FocusControl = edPath
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblExplainMove: TLabel
      Left = 0
      Top = 171
      Width = 361
      Height = 62
      AutoSize = False
      Caption = 
        'The directory must be empty and must not be a sub-directory of t' +
        'he current collection'#39's data directory. If the directory does no' +
        't exist a new one will be created.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblCollection: TLabel
      Left = 0
      Top = 71
      Width = 122
      Height = 13
      Caption = 'Select &collection to move:'
      FocusControl = cbCollection
    end
    inline frmProgress: TProgressFrame
      Left = 57
      Top = 0
      Width = 320
      Height = 82
      ParentBackground = False
      TabOrder = 4
      Visible = False
      ExplicitLeft = 57
      ExplicitHeight = 82
      inherited pnlBody: TPanel
        Height = 82
        ExplicitLeft = 55
        ExplicitTop = 72
        ExplicitHeight = 82
      end
    end
    object btnMove: TButton
      Left = 87
      Top = 199
      Width = 153
      Height = 41
      Action = actMove
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object edPath: TEdit
      Left = 0
      Top = 144
      Width = 325
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object btnBrowse: TButton
      Left = 331
      Top = 144
      Width = 27
      Height = 21
      Action = actBrowse
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object cbCollection: TComboBox
      Left = 0
      Top = 88
      Width = 358
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object alDlg: TActionList
    Left = 152
    Top = 304
    object actBrowse: TAction
      Caption = '...'
      OnExecute = actBrowseExecute
    end
    object actMove: TAction
      Caption = '&Move'
      OnExecute = actMoveExecute
      OnUpdate = actMoveUpdate
    end
  end
end
