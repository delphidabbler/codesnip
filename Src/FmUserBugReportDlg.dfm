inherited UserBugReportDlg: TUserBugReportDlg
  Caption = 'Report Bug Online'
  ExplicitWidth = 967
  ExplicitHeight = 820
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    inherited lblBugTracker: TLabel
      Top = 233
      ExplicitTop = 233
    end
    object lblInstruct1: TLabel [1]
      Left = 7
      Top = 7
      Width = 772
      Height = 67
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Thanks for taking the time to report a bug.'
      WordWrap = True
    end
    object lblInstruct2: TLabel [2]
      Left = 7
      Top = 56
      Width = 772
      Height = 95
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'Please use the link below to display CodeSnip'#39's online bug track' +
        'er on GitHub (GitHub account required). Please review the existi' +
        'ng bug reports to see if anything similar has been reported alre' +
        'ady, or maybe even fixed.'
      WordWrap = True
    end
    inherited lblBugTrackerKey: TLabel
      Left = 225
      Top = 233
      ExplicitLeft = 225
      ExplicitTop = 233
    end
    object lblInstruct3: TLabel
      Left = 7
      Top = 161
      Width = 772
      Height = 58
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'If the bug has not been reported please create a new bug report ' +
        'and provide as much information as you can.'
      WordWrap = True
    end
  end
  inherited alMain: TActionList
    Left = 36
  end
end
