inherited TestCompileDlg: TTestCompileDlg
  Caption = 'Test Compile Results'
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
    Top = 16
    Width = 366
    Height = 2637
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 16
    ExplicitWidth = 366
    ExplicitHeight = 2637
    object lblSnippetName: TLabel
      Left = 168
      Top = 14
      Width = 152
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'lblSnippetName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSnippetNameDesc: TLabel
      Left = 0
      Top = 14
      Width = 978
      Height = 265
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Results for '
    end
    object sbCompilers: TScrollBox
      Left = 0
      Top = 47
      Width = 352
      Height = 436
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      BevelInner = bvNone
      BevelOuter = bvRaised
      BevelKind = bkFlat
      BorderStyle = bsNone
      TabOrder = 0
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
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
  end
  object btnViewErrors: TButton
    Left = 14
    Top = 532
    Width = 268
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actViewErrors
    TabOrder = 3
  end
  object alMain: TActionList
    Left = 256
    Top = 16
    object actScrollPageUp: TAction
      Caption = 'actScrollPageUp'
      ShortCut = 33
      OnExecute = actScrollPageUpExecute
    end
    object actScrollPageDown: TAction
      Caption = 'actScrollPageDown'
      ShortCut = 34
      OnExecute = actScrollPageDownExecute
    end
    object actScrollLineUp: TAction
      Caption = 'actScrollLineUp'
      ShortCut = 38
      OnExecute = actScrollLineUpExecute
    end
    object actScrollLineDown: TAction
      Caption = 'actScrollLineDown'
      ShortCut = 40
      OnExecute = actScrollLineDownExecute
    end
    object actViewErrors: TAction
      Caption = '&View Errors && Warnings...'
      OnExecute = actViewErrorsExecute
      OnUpdate = actViewErrorsUpdate
    end
  end
end
