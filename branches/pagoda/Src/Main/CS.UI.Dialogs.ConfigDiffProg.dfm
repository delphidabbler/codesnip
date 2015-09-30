inherited ConfigDiffProgDlg: TConfigDiffProgDlg
  Caption = 'Configure Diff Viewer Program'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 441
    ExplicitWidth = 441
    object lblProgFile: TLabel
      Left = 0
      Top = 0
      Width = 189
      Height = 13
      Caption = 'Enter Diff program file name (full path):'
    end
    object lblParams: TLabel
      Left = 0
      Top = 56
      Width = 131
      Height = 13
      Caption = 'Enter required parameters:'
    end
    object lblParamsHelp: TLabel
      Left = 0
      Top = 102
      Width = 239
      Height = 13
      Caption = 'Specify %1 for first file name and %2 for second.'
    end
    object edProgFile: TEdit
      Left = 0
      Top = 19
      Width = 337
      Height = 21
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 352
      Top = 19
      Width = 27
      Height = 21
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object edParams: TEdit
      Left = 0
      Top = 75
      Width = 233
      Height = 21
      TabOrder = 2
    end
    object btnTest: TButton
      Left = 0
      Top = 172
      Width = 75
      Height = 25
      Action = actTest
      TabOrder = 3
    end
    object btnClear: TButton
      Left = 104
      Top = 172
      Width = 75
      Height = 25
      Action = actClear
      TabOrder = 4
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
  object alDlg: TActionList
    Left = 224
    Top = 176
    object actClear: TAction
      Caption = 'Clear'
      OnExecute = actClearExecute
      OnUpdate = actClearUpdate
    end
    object actTest: TAction
      Caption = 'Test Diff'
      OnExecute = actTestExecute
      OnUpdate = actTestUpdate
    end
  end
end
