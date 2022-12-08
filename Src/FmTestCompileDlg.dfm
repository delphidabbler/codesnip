inherited TestCompileDlg: TTestCompileDlg
  Caption = 'Test Compile Results'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblSnippetName: TLabel
      Left = 168
      Top = 14
      Width = 159
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'lblSnippetName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSnippetNameDesc: TLabel
      Left = 0
      Top = 14
      Width = 103
      Height = 30
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
    Left = 424
    Top = 128
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
