inherited TrappedBugReportDlg: TTrappedBugReportDlg
  Caption = 'Unexpected Error'
  ExplicitHeight = 356
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblIntro: TLabel [0]
      Left = 4
      Top = 4
      Width = 309
      Height = 13
      Caption = 'CodeSnip has detected the following unexpected error:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bvlBugDesc: TBevel [1]
      Left = 4
      Top = 19
      Width = 441
      Height = 34
      Shape = bsFrame
    end
    object lblBugInfo: TLabel [2]
      Left = 12
      Top = 25
      Width = 425
      Height = 24
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblInstruct1: TLabel [3]
      Left = 5
      Top = 59
      Width = 441
      Height = 38
      AutoSize = False
      Caption = 
        'It will be helpful if you can take the time to report this bug u' +
        'sing the online CodeSnip bug tracker.'
      WordWrap = True
    end
    object lblInstruct2: TLabel [4]
      Left = 4
      Top = 91
      Width = 441
      Height = 46
      AutoSize = False
      Caption = 
        'Please use the link below to display the bug tracker and then re' +
        'view the existing bug reports to see if anything similar has bee' +
        'n reported already. If not please create a new bug report and pr' +
        'ovide as much information as you can.'
      WordWrap = True
    end
    object lblInstruct3: TLabel [5]
      Left = 4
      Top = 143
      Width = 441
      Height = 38
      AutoSize = False
      Caption = 
        'CodeSnip has gathered some information about the bug that will b' +
        'e placed on the clipboard. When you are reporting the bug please' +
        ' paste the information after your description of the problem.'
      WordWrap = True
    end
    inherited lblBugTrackerKey: TLabel
      Left = 116
      ExplicitLeft = 116
    end
    object lblInstruct4: TLabel
      Left = 4
      Top = 187
      Width = 441
      Height = 38
      AutoSize = False
      Caption = 
        'This bug may have left the program in an unstable state. You are' +
        ' advised to terminate the application and restart it. Click "Ter' +
        'minate CodeSnip" to close the program now. CodeSnip will attempt' +
        ' to save any database changes before closing. If you prefer to l' +
        'eave the application open click "Continue".'
      WordWrap = True
    end
  end
  inherited btnHelp: TButton
    TabOrder = 3
  end
  inherited btnClose: TButton
    Caption = '&Continue'
    Default = False
    TabOrder = 2
  end
  object btnTerminate: TButton [4]
    Left = 64
    Top = 304
    Width = 135
    Height = 25
    Action = actTerminate
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  inherited alMain: TActionList
    object actTerminate: TAction
      Caption = '&Terminate CodeSnip'
      OnExecute = actTerminateExecute
    end
  end
end
