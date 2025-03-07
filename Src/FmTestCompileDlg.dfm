inherited TestCompileDlg: TTestCompileDlg
  Caption = 'Test Compile Results'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Top = 9
    Width = 209
    ExplicitTop = 9
    ExplicitWidth = 209
    object lblSnippetName: TLabel
      Left = 96
      Top = 8
      Width = 88
      Height = 13
      Caption = 'lblSnippetName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSnippetNameDesc: TLabel
      Left = 0
      Top = 8
      Width = 55
      Height = 13
      Caption = 'Results for '
    end
    object sbCompilers: TScrollBox
      Left = 0
      Top = 27
      Width = 201
      Height = 249
      BevelInner = bvNone
      BevelOuter = bvRaised
      BevelKind = bkFlat
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object btnViewErrors: TButton
    Left = 8
    Top = 304
    Width = 153
    Height = 25
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
