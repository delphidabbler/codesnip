inherited UserBugReportDlg: TUserBugReportDlg
  Caption = 'Report Bug Online'
  ClientHeight = 3229
  ClientWidth = 4512
  Font.Height = -350
  ExplicitWidth = 4536
  ExplicitHeight = 3293
  PixelsPerInch = 168
  TextHeight = 466
  inherited bvlBottom: TBevel
    Left = 135
    Top = 4860
    Width = 6190
    Height = 40
    Margins.Left = 49
    Margins.Top = 49
    Margins.Right = 49
    Margins.Bottom = 49
    ExplicitLeft = 135
    ExplicitTop = 4860
    ExplicitWidth = 6190
    ExplicitHeight = 40
  end
  inherited pnlBody: TPanel
    Left = 135
    Top = 16
    Width = 1375
    Height = 4615
    Margins.Left = 49
    Margins.Top = 49
    Margins.Right = 49
    Margins.Bottom = 49
    ExplicitLeft = 135
    ExplicitTop = 16
    ExplicitWidth = 1375
    ExplicitHeight = 4615
    inherited lblBugTracker: TLabel
      Left = 12
      Top = 784
      Width = 318
      Height = 40
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      Font.Height = -33
      ExplicitLeft = 12
      ExplicitTop = 784
      ExplicitWidth = 318
      ExplicitHeight = 40
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
      Left = 319
      Top = 784
      Width = 1082
      Height = 466
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Margins.Bottom = 9
      ExplicitLeft = 319
      ExplicitTop = 784
      ExplicitWidth = 1082
      ExplicitHeight = 466
    end
    object lblInstruct3: TLabel
      Left = 7
      Top = 126
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
  inherited btnHelp: TButton
    Left = 5124
    Top = 4989
    Width = 1229
    Height = 413
    Margins.Left = 49
    Margins.Top = 49
    Margins.Right = 49
    Margins.Bottom = 49
    ExplicitLeft = 5124
    ExplicitTop = 4989
    ExplicitWidth = 1229
    ExplicitHeight = 413
  end
  inherited btnClose: TButton
    Left = 404
    Top = 1629
    Width = 401
    Height = 135
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    ExplicitLeft = 404
    ExplicitTop = 1629
    ExplicitWidth = 401
    ExplicitHeight = 135
  end
end
