inherited TrappedBugReportDlg: TTrappedBugReportDlg
  Caption = 'Unexpected Error'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblIntro: TLabel [0]
      Left = 4
      Top = 4
      Width = 313
      Height = 13
      Caption = 'CodeSnip has detected the following unexpected error:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
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
      Font.Name = 'MS Sans Serif'
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
      Top = 175
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
  end
end
