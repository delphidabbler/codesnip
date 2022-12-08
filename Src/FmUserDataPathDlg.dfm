inherited UserDataPathDlg: TUserDataPathDlg
  Caption = 'Move User Database'
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Left = 77
    Top = 16
    Width = 3537
    Height = 576
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 16
    ExplicitWidth = 3537
    ExplicitHeight = 576
    object lblInstructions: TLabel
      Left = 0
      Top = 0
      Width = 660
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'Use this dialogue box to move the user database to a new directo' +
        'ry or to restore the directory to its default location. Choose t' +
        'he appropriate option below.'
      WordWrap = True
    end
    object lblWarning: TLabel
      Left = 0
      Top = 56
      Width = 660
      Height = 35
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'You are strongly advised to make a backup of the database before' +
        ' continuing.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object gbMove: TGroupBox
      Left = 0
      Top = 103
      Width = 660
      Height = 245
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Move database to new directory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lblPath: TLabel
        Left = 14
        Top = 35
        Width = 454
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Enter the full path to the new database &directory:'
        FocusControl = edPath
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblExplainMove: TLabel
        Left = 14
        Top = 116
        Width = 632
        Height = 59
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        AutoSize = False
        Caption = 
          'The directory must be empty and must not be a sub-directory of t' +
          'he current database directory. If the directory does not exist a' +
          ' new one will be created.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object edPath: TEdit
        Left = 14
        Top = 68
        Width = 569
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object btnBrowse: TButton
        Left = 597
        Top = 68
        Width = 47
        Height = 37
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Action = actBrowse
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object btnMove: TButton
        Left = 196
        Top = 151
        Width = 268
        Height = 71
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Action = actMove
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object gbRestore: TGroupBox
      Left = 0
      Top = 364
      Width = 660
      Height = 196
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Restore database to default directory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object lblExplainDefaultPath: TLabel
        Left = 14
        Top = 35
        Width = 632
        Height = 60
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        AutoSize = False
        Caption = 
          'Use this button to restore the database to its default directory' +
          '. This option is only available if the database has been moved.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object btnDefaultPath: TButton
        Left = 196
        Top = 107
        Width = 268
        Height = 72
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Action = actDefaultPath
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    inline frmProgress: TProgressFrame
      Left = 57
      Top = 0
      Width = 320
      Height = 82
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 2
      Visible = False
      ExplicitLeft = 57
      ExplicitWidth = 320
      ExplicitHeight = 82
      inherited pnlBody: TPanel
        Width = 320
        Height = 82
        ExplicitWidth = 320
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
