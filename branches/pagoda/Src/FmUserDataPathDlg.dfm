inherited DBMoveDlg: TDBMoveDlg
  Caption = 'Move Database'
  ExplicitWidth = 474
  ExplicitHeight = 374
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
        'Use this dialogue box to move the database to a new directory or' +
        ' to restore the directory to its default location. Choose the ap' +
        'propriate option below.'
      WordWrap = True
    end
    object lblWarning: TLabel
      Left = 0
      Top = 32
      Width = 377
      Height = 20
      AutoSize = False
      Caption = 
        'You are strongly advised to make a backup of the database before' +
        ' continuing.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object gbMove: TGroupBox
      Left = 0
      Top = 59
      Width = 377
      Height = 140
      Caption = 'Move database to new directory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lblPath: TLabel
        Left = 8
        Top = 20
        Width = 240
        Height = 13
        Caption = 'Enter the full path to the new database &directory:'
        FocusControl = edPath
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblExplainMove: TLabel
        Left = 8
        Top = 66
        Width = 361
        Height = 34
        AutoSize = False
        Caption = 
          'The directory must be empty and must not be a sub-directory of t' +
          'he current database directory. If the directory does not exist a' +
          ' new one will be created.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object edPath: TEdit
        Left = 8
        Top = 39
        Width = 325
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object btnBrowse: TButton
        Left = 341
        Top = 39
        Width = 27
        Height = 21
        Action = actBrowse
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object btnMove: TButton
        Left = 112
        Top = 86
        Width = 153
        Height = 41
        Action = actMove
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object gbRestore: TGroupBox
      Left = 0
      Top = 208
      Width = 377
      Height = 112
      Caption = 'Restore database to default directory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object lblExplainDefaultPath: TLabel
        Left = 8
        Top = 20
        Width = 361
        Height = 34
        AutoSize = False
        Caption = 
          'Use this button to restore the database to its default directory' +
          '. This option is only available if the database has been moved.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object btnDefaultPath: TButton
        Left = 112
        Top = 61
        Width = 153
        Height = 41
        Action = actDefaultPath
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    inline frmProgress: TDBMoveDlgProgressFrame
      Left = 57
      Top = 0
      Width = 320
      Height = 82
      ParentBackground = False
      TabOrder = 2
      Visible = False
      ExplicitLeft = 57
      ExplicitHeight = 82
      inherited pnlBody: TPanel
        Height = 82
        ExplicitHeight = 82
      end
    end
  end
  object alDlg: TActionList
    Left = 152
    Top = 304
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
      Caption = '&Move'
      OnExecute = actMoveExecute
      OnUpdate = actMoveUpdate
    end
  end
end
