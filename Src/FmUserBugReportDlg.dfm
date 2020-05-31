inherited UserBugReportDlg: TUserBugReportDlg
  Caption = 'Report Bug Online'
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Top = 9
    ExplicitTop = 9
    object lblInstruct1: TLabel [1]
      Left = 4
      Top = 4
      Width = 441
      Height = 38
      AutoSize = False
      Caption = 'Thanks for taking the time to report a bug.'
      WordWrap = True
    end
    object lblInstruct2: TLabel [2]
      Left = 4
      Top = 32
      Width = 441
      Height = 54
      AutoSize = False
      Caption = 
        'Please use the link below to display CodeSnip'#39's online bug track' +
        'er on GitHub (GitHub account required). Please review the existi' +
        'ng bug reports to see if anything similar has been reported alre' +
        'ady, or maybe even fixed.'
      WordWrap = True
    end
    object lblInstruct3: TLabel
      Left = 4
      Top = 72
      Width = 441
      Height = 33
      AutoSize = False
      Caption = 
        'If the bug has not been reported please create a new bug report ' +
        'and provide as much information as you can.'
      WordWrap = True
    end
  end
  inherited btnClose: TButton
    Left = 231
    ExplicitLeft = 231
  end
end
