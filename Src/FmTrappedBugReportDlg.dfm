inherited TrappedBugReportDlg: TTrappedBugReportDlg
  Caption = 'Unexpected Error'
  ClientWidth = 967
  ExplicitHeight = 820
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblIntro: TLabel [0]
      Left = 7
      Top = 7
      Width = 545
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'CodeSnip has detected the following unexpected error:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bvlBugDesc: TBevel [1]
      Left = 7
      Top = 33
      Width = 772
      Height = 60
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Shape = bsFrame
    end
    object lblBugInfo: TLabel [2]
      Left = 21
      Top = 44
      Width = 744
      Height = 42
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblInstruct1: TLabel [3]
      Left = 9
      Top = 103
      Width = 772
      Height = 67
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'It will be helpful if you can take the time to report this bug u' +
        'sing the online CodeSnip bug tracker on GitHub (GitHub account r' +
        'equired).'
      WordWrap = True
    end
    object lblInstruct2: TLabel [4]
      Left = 7
      Top = 159
      Width = 772
      Height = 81
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'Please use the link below to display the bug tracker and then re' +
        'view the existing bug reports to see if anything similar has bee' +
        'n reported already. If not please create a new bug report and pr' +
        'ovide as much information as you can.'
      WordWrap = True
    end
    object lblInstruct3: TLabel [5]
      Left = 7
      Top = 250
      Width = 772
      Height = 67
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'CodeSnip has gathered some data about the bug that will be place' +
        'd on the clipboard. When you are reporting the bug please paste ' +
        'the information after your description of the problem. Please do' +
        ' write a description of what you were doing. The data on the cli' +
        'pboard is not sufficient on its own.'
      WordWrap = True
    end
    inherited lblBugTracker: TLabel
      Left = 12
      Top = 400
      ExplicitLeft = 12
      ExplicitTop = 400
    end
    inherited lblBugTrackerKey: TLabel
      Left = 231
      Top = 400
      ExplicitLeft = 231
      ExplicitTop = 400
    end
    object lblInstruct4: TLabel
      Left = 7
      Top = 327
      Width = 772
      Height = 67
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
    Left = 112
    Top = 532
    Width = 236
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actTerminate
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
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
